// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox

import java.io.FileWriter
import java.time.Instant

import akka.stream.ActorMaterializer
import com.daml.ledger.participant.state.v1.ParticipantId
import com.digitalasset.api.util.TimeProvider
import com.digitalasset.daml.lf.data.{ImmArray, Ref}
import com.digitalasset.daml.lf.engine.Engine
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.ledger.api.auth.interceptor.AuthorizationInterceptor
import com.digitalasset.ledger.api.auth.{AuthServiceWildcard, Authorizer}
import com.digitalasset.ledger.api.domain.LedgerId
import com.digitalasset.ledger.server.apiserver.{ApiServices, LedgerApiServer}
import com.digitalasset.platform.common.LedgerIdMode
import com.digitalasset.platform.common.logging.NamedLoggerFactory
import com.digitalasset.platform.sandbox.SandboxServer._
import com.digitalasset.platform.sandbox.banner.Banner
import com.digitalasset.platform.sandbox.config.SandboxConfig
import com.digitalasset.platform.sandbox.services.SandboxResetService
import com.digitalasset.platform.sandbox.stores.ledger.ScenarioLoader.LedgerEntryOrBump
import com.digitalasset.platform.sandbox.stores.ledger._
import com.digitalasset.platform.sandbox.stores.ledger.sql.SqlStartMode
import com.digitalasset.platform.sandbox.stores.{
  InMemoryActiveLedgerState,
  InMemoryPackageStore,
  SandboxIndexAndWriteService
}
import com.digitalasset.platform.server.services.testing.TimeServiceBackend
import com.digitalasset.platform.services.time.TimeProviderType
import org.slf4j.LoggerFactory

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class RunningSandboxServer(
    config: SandboxConfig,
    infrastructure: Infrastructure,
    packageStore: InMemoryPackageStore,
    apiServerState: ApiServerState,
    resetServer: () => Future[Unit],
) extends AutoCloseable {
  def port: Int = apiServerState.port

  def closed(): Future[Unit] =
    apiServerState.apiServer.servicesClosed()

  override def close(): Unit =
    apiServerState.close()
}

object RunningSandboxServer {
  private val logger = LoggerFactory.getLogger(this.getClass)

  // Name of this participant
  // TODO: Pass this info in command-line (See issue #2025)
  val participantId: ParticipantId = Ref.LedgerString.assertFromString("sandbox-participant")

  // We memoize the engine between resets so we avoid the expensive
  // repeated validation of the sames packages after each reset
  private val engine = Engine()

  def create(
      config: SandboxConfig,
      infrastructure: Infrastructure,
      packageStore: InMemoryPackageStore,
      resetServer: () => Future[Unit],
  ): RunningSandboxServer =
    apply(
      config,
      infrastructure,
      packageStore,
      SqlStartMode.ContinueIfExists,
      resetServer,
      currentPort = None)

  def recreate(
      config: SandboxConfig,
      infrastructure: Infrastructure,
      packageStore: InMemoryPackageStore,
      currentPort: Int,
      resetServer: () => Future[Unit],
  ): RunningSandboxServer =
    apply(
      config,
      infrastructure,
      packageStore,
      SqlStartMode.AlwaysReset,
      resetServer,
      Some(currentPort),
    )

  private def apply(
      config: SandboxConfig,
      infrastructure: Infrastructure,
      packageStore: InMemoryPackageStore,
      startMode: SqlStartMode,
      resetServer: () => Future[Unit],
      currentPort: Option[Int],
  ): RunningSandboxServer = {
    implicit val materializer: ActorMaterializer = infrastructure.materializer
    implicit val executionContext: ExecutionContext = infrastructure.executionContext

    val ledgerId = config.ledgerIdMode match {
      case LedgerIdMode.Static(id) => id
      case LedgerIdMode.Dynamic() => LedgerIdGenerator.generateRandomId()
    }

    val (acs, ledgerEntries, mbLedgerTime) = createInitialState(config, packageStore)

    val (timeProvider, timeServiceBackendO: Option[TimeServiceBackend]) =
      (mbLedgerTime, config.timeProviderType) match {
        case (None, TimeProviderType.WallClock) => (TimeProvider.UTC, None)
        case (ledgerTime, _) =>
          val ts = TimeServiceBackend.simple(
            ledgerTime.getOrElse(Instant.EPOCH),
            config.timeProviderType == TimeProviderType.StaticAllowBackwards)
          (ts, Some(ts))
      }

    val loggerFactory = NamedLoggerFactory.forParticipant(participantId)

    val (ledgerType, indexAndWriteServiceF) = config.jdbcUrl match {
      case Some(jdbcUrl) =>
        "postgres" -> SandboxIndexAndWriteService.postgres(
          ledgerId,
          participantId,
          jdbcUrl,
          config.timeModel,
          timeProvider,
          acs,
          ledgerEntries,
          startMode,
          config.commandConfig.maxCommandsInFlight * 2, // we can get commands directly as well on the submission service
          packageStore,
          loggerFactory,
          infrastructure.metricsManager,
        )
      case None =>
        "in-memory" -> Future.successful(
          SandboxIndexAndWriteService.inMemory(
            ledgerId,
            participantId,
            config.timeModel,
            timeProvider,
            acs,
            ledgerEntries,
            packageStore,
            infrastructure.metricsManager,
          ))
    }

    val indexAndWriteService = Try(Await.result(indexAndWriteServiceF, Async.tolerance))
      .fold(t => {
        val msg = "Could not create SandboxIndexAndWriteService"
        logger.error(msg, t)
        sys.error(msg)
      }, identity)

    val authService = config.authService.getOrElse(AuthServiceWildcard)
    val authorizer = new Authorizer(() => java.time.Clock.systemUTC.instant())

    val resetService =
      newResetService(infrastructure, ledgerId, authorizer, loggerFactory, resetServer)
    val apiServer = Await.result(
      LedgerApiServer.create(
        (am: ActorMaterializer, esf: ExecutionSequencerFactory) =>
          ApiServices
            .create(
              indexAndWriteService.writeService,
              indexAndWriteService.indexService,
              authorizer,
              engine,
              timeProvider,
              config.timeModel,
              config.commandConfig,
              timeServiceBackendO.map(
                TimeServiceBackend.withObserver(_, indexAndWriteService.publishHeartbeat)),
              loggerFactory,
            )(am, esf)
            .map(_.withServices(List(resetService))),
        currentPort.getOrElse(config.port),
        config.maxInboundMessageSize,
        config.address,
        loggerFactory,
        config.tlsConfig.flatMap(_.server),
        List(
          AuthorizationInterceptor(authService, executionContext),
          resetService,
        ),
      ),
      Async.tolerance,
    )

    val newState = ApiServerState(
      ledgerId,
      apiServer,
      indexAndWriteService,
    )

    Banner.show(Console.out)
    logger.info(
      "Initialized sandbox version {} with ledger-id = {}, port = {}, dar file = {}, time mode = {}, ledger = {}, auth-service = {}",
      BuildInfo.Version,
      ledgerId,
      newState.port.toString,
      config.damlPackages,
      config.timeProviderType,
      ledgerType,
      authService.getClass.getSimpleName,
    )

    writePortFile(config, newState.port)

    new RunningSandboxServer(
      config,
      infrastructure,
      packageStore,
      newState,
      resetServer,
    )
  }

  // if requested, initialize the ledger state with the given scenario
  private def createInitialState(config: SandboxConfig, packageStore: InMemoryPackageStore)
    : (InMemoryActiveLedgerState, ImmArray[LedgerEntryOrBump], Option[Instant]) = {
    // [[ScenarioLoader]] needs all the packages to be already compiled --
    // make sure that that's the case
    if (config.eagerPackageLoading || config.scenario.nonEmpty) {
      for (pkgId <- packageStore.listLfPackagesSync().keys) {
        val pkg = packageStore.getLfPackageSync(pkgId).get
        engine
          .preloadPackage(pkgId, pkg)
          .consume(
            { _ =>
              sys.error("Unexpected request of contract")
            },
            packageStore.getLfPackageSync, { _ =>
              sys.error("Unexpected request of contract key")
            }
          )
      }
    }
    config.scenario match {
      case None => (InMemoryActiveLedgerState.empty, ImmArray.empty, None)
      case Some(scenario) =>
        val (acs, records, ledgerTime) =
          ScenarioLoader.fromScenario(packageStore, engine.compiledPackages(), scenario)
        (acs, records, Some(ledgerTime))
    }
  }

  /** the reset service is special, since it triggers a server shutdown */
  private def newResetService(
      infrastructure: Infrastructure,
      ledgerId: LedgerId,
      authorizer: Authorizer,
      loggerFactory: NamedLoggerFactory,
      reset: () => Future[Unit]): SandboxResetService =
    new SandboxResetService(
      ledgerId,
      () => infrastructure.executionContext,
      reset,
      authorizer,
      loggerFactory
    )

  private def writePortFile(config: SandboxConfig, port: Int): Unit = {
    config.portFile.foreach { f =>
      val w = new FileWriter(f)
      w.write(s"$port\n")
      w.close()
    }
  }
}
