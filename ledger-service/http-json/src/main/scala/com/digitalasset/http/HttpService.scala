// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.http

import akka.actor.{ActorSystem, Cancellable}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.settings.ServerSettings
import akka.stream.Materializer
import com.digitalasset.api.util.TimeProvider
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.http.dbbackend.ContractDao
import com.digitalasset.http.json.{
  ApiValueToJsValueConverter,
  DomainJsonDecoder,
  DomainJsonEncoder,
  JsValueToApiValueConverter
}
import com.digitalasset.http.util.ApiValueToLfValueConverter
import com.digitalasset.http.util.FutureUtil._
import com.digitalasset.http.util.IdentifierConverters.apiLedgerId
import com.digitalasset.jwt.JwtDecoder
import com.digitalasset.ledger.api.refinements.ApiTypes.ApplicationId
import com.digitalasset.ledger.api.refinements.{ApiTypes => lar}
import com.digitalasset.ledger.client.LedgerClient
import com.digitalasset.ledger.client.configuration.{
  CommandClientConfiguration,
  LedgerClientConfiguration,
  LedgerIdRequirement
}
import com.digitalasset.ledger.client.services.pkg.PackageClient
import com.digitalasset.ledger.service.LedgerReader
import com.typesafe.scalalogging.StrictLogging
import io.grpc.netty.NettyChannelBuilder
import scalaz.Scalaz._
import scalaz._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object HttpService extends StrictLogging {

  val DefaultPackageReloadInterval: FiniteDuration = FiniteDuration(5, "s")
  val DefaultMaxInboundMessageSize: Int = 4194304

  private type ET[A] = EitherT[Future, Error, A]

  final case class Error(message: String)

  def start(
      ledgerHost: String,
      ledgerPort: Int,
      applicationId: ApplicationId,
      address: String,
      httpPort: Int,
      contractDao: Option[ContractDao] = None,
      staticContentConfig: Option[StaticContentConfig] = None,
      packageReloadInterval: FiniteDuration = DefaultPackageReloadInterval,
      maxInboundMessageSize: Int = DefaultMaxInboundMessageSize,
      validateJwt: Endpoints.ValidateJwt = decodeJwt)(
      implicit asys: ActorSystem,
      mat: Materializer,
      aesf: ExecutionSequencerFactory,
      ec: ExecutionContext
  ): Future[Error \/ ServerBinding] = {

    implicit val settings: ServerSettings = ServerSettings(asys)

    val clientConfig = LedgerClientConfiguration(
      applicationId = ApplicationId.unwrap(applicationId),
      ledgerIdRequirement = LedgerIdRequirement("", enabled = false),
      commandClient = CommandClientConfiguration.default,
      sslContext = None
    )

    val bindingEt: EitherT[Future, Error, ServerBinding] = for {
      client <- eitherT(client(ledgerHost, ledgerPort, clientConfig, maxInboundMessageSize)): ET[
        LedgerClient]

      ledgerId = apiLedgerId(client.ledgerId): lar.LedgerId

      _ = logger.info(s"Connected to Ledger: ${ledgerId: lar.LedgerId}")
      _ = logger.info(s"contractDao: ${contractDao.toString}")

      // TODO Pass a token to work against a ledger with authentication
      packageService = new PackageService(loadPackageStoreUpdates(client.packageClient, None))

      // load all packages right away
      _ <- eitherT(packageService.reload).leftMap(e => Error(e.shows)): ET[Unit]

      _ = schedulePackageReload(packageService, packageReloadInterval)

      commandService = new CommandService(
        packageService.resolveTemplateId,
        packageService.resolveChoiceRecordId,
        LedgerClientJwt.submitAndWaitForTransaction(client),
        TimeProvider.UTC
      )

      contractsService = new ContractsService(
        packageService.resolveTemplateIds,
        LedgerClientJwt.getActiveContracts(client),
        LedgerClientJwt.getCreatesAndArchivesSince(client),
        LedgerReader.damlLfTypeLookup(packageService.packageStore _),
        contractDao,
      )

      partiesService = new PartiesService(() => client.partyManagementClient.listKnownParties())

      (encoder, decoder) = buildJsonCodecs(ledgerId, packageService)

      jsonEndpoints = new Endpoints(
        ledgerId,
        validateJwt,
        commandService,
        contractsService,
        partiesService,
        encoder,
        decoder,
      )

      allEndpoints = staticContentConfig.cata(
        c => StaticContentEndpoints.all(c) orElse jsonEndpoints.all,
        jsonEndpoints.all
      )

      binding <- liftET[Error](
        Http().bindAndHandleAsync(allEndpoints, address, httpPort, settings = settings))

    } yield binding

    bindingEt.run: Future[Error \/ ServerBinding]
  }

  private[http] def loadPackageStoreUpdates(packageClient: PackageClient, token: Option[String])(
      implicit ec: ExecutionContext): PackageService.ReloadPackageStore =
    (ids: Set[String]) =>
      LedgerReader
        .loadPackageStoreUpdates(packageClient, token)(ids)
        .map(_.leftMap(e => PackageService.ServerError(e)))

  def stop(f: Future[Error \/ ServerBinding])(implicit ec: ExecutionContext): Future[Unit] = {
    logger.info("Stopping server...")
    f.collect { case \/-(a) => a.unbind().void }.join
  }

  // Decode JWT without any validation
  private val decodeJwt: Endpoints.ValidateJwt =
    jwt => JwtDecoder.decode(jwt).leftMap(e => Endpoints.Unauthorized(e.shows))

  private[http] def buildJsonCodecs(
      ledgerId: lar.LedgerId,
      packageService: PackageService): (DomainJsonEncoder, DomainJsonDecoder) = {

    val lfTypeLookup = LedgerReader.damlLfTypeLookup(packageService.packageStore _) _
    val jsValueToApiValueConverter = new JsValueToApiValueConverter(lfTypeLookup)
    val jsObjectToApiRecord = jsValueToApiValueConverter.jsObjectToApiRecord _
    val jsValueToApiValue = jsValueToApiValueConverter.jsValueToApiValue _
    val apiValueToJsValueConverter = new ApiValueToJsValueConverter(
      ApiValueToLfValueConverter.apiValueToLfValue)
    val apiValueToJsValue = apiValueToJsValueConverter.apiValueToJsValue _
    val apiRecordToJsObject = apiValueToJsValueConverter.apiRecordToJsObject _

    val encoder = new DomainJsonEncoder(apiRecordToJsObject, apiValueToJsValue)
    val decoder = new DomainJsonDecoder(
      packageService.resolveTemplateId,
      packageService.resolveChoiceRecordId,
      jsObjectToApiRecord,
      jsValueToApiValue)

    (encoder, decoder)
  }

  private def schedulePackageReload(packageService: PackageService, pollInterval: FiniteDuration)(
      implicit asys: ActorSystem,
      ec: ExecutionContext): Cancellable =
    asys.scheduler.schedule(pollInterval, pollInterval) {
      val f: Future[PackageService.Error \/ Unit] = packageService.reload
      f.onComplete {
        case scala.util.Failure(e) => logger.error("Package reload failed", e)
        case scala.util.Success(-\/(e)) => logger.error("Package reload failed: " + e.shows)
        case scala.util.Success(\/-(_)) =>
      }
    }

  private def client(
      ledgerHost: String,
      ledgerPort: Int,
      clientConfig: LedgerClientConfiguration,
      maxInboundMessageSize: Int)(
      implicit ec: ExecutionContext,
      aesf: ExecutionSequencerFactory): Future[Error \/ LedgerClient] =
    LedgerClient
      .fromBuilder(
        NettyChannelBuilder
          .forAddress(ledgerHost, ledgerPort)
          .maxInboundMessageSize(maxInboundMessageSize),
        clientConfig)
      .map(_.right)
      .recover {
        case NonFatal(e) =>
          \/.left(Error(s"Cannot connect to the ledger server, error: ${e.getMessage}"))
      }
}
