// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox

import java.io.File
import java.time.Instant

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.digitalasset.ledger.api.domain.LedgerId
import com.digitalasset.ledger.server.apiserver.ApiServer
import com.digitalasset.platform.sandbox.SandboxServer._
import com.digitalasset.platform.sandbox.config.SandboxConfig
import com.digitalasset.platform.sandbox.metrics.MetricsManager
import com.digitalasset.platform.sandbox.stores.{InMemoryPackageStore, IndexAndWriteService}
import org.slf4j.LoggerFactory

import scala.concurrent.{Await, ExecutionContext, Future}

class SandboxServer(config: SandboxConfig, actorSystemName: String = defaultActorSystemName)
    extends AutoCloseable {
  private val infrastructure = Infrastructure(actorSystemName)

  private lazy val packageStore = loadDamlPackages()

  @volatile private var runningServer: Option[RunningSandboxServer] = None

  def start(): Unit = {
    runningServer = Some(
      RunningSandboxServer
        .create(config, infrastructure, packageStore, () => resetAndRestartServer()))
  }

  def port: Int = runningServer.get.port

  override def close(): Unit = {
    runningServer.foreach(_.close())
    infrastructure.close()
  }

  private def resetAndRestartServer(): Future[Unit] = {
    implicit val ec: ExecutionContext = infrastructure.executionContext

    // Need to run this async otherwise the callback kills the server under the
    // in-flight reset service request!
    Future {
      logger.debug("Tearing down the old server...")
      runningServer.get.close()

      logger.debug("Constructing a new server...")
      val newRunningServer = RunningSandboxServer.recreate(
        config,
        infrastructure,
        packageStore,
        runningServer.get.port,
        () => resetAndRestartServer(),
      )
      // TODO: eliminate the state mutation somehow
      // yes, it's horrible that we mutate the state here, but believe me, it's
      // still an improvement to what we had before!
      runningServer = Some(newRunningServer)
      logger.debug("The server was reset and restarted.")
    }

    // waits for the services to be closed, so we can guarantee that future API
    // calls after finishing the reset will never be handled by the old one
    runningServer.get.closed()
  }

  private def loadDamlPackages(): InMemoryPackageStore = {
    // TODO is it sensible to have all the initial packages to be known since the epoch?
    config.damlPackages
      .foldLeft[Either[(String, File), InMemoryPackageStore]](Right(InMemoryPackageStore.empty)) {
        case (storeE, f) =>
          storeE.right.flatMap(_.withDarFile(Instant.EPOCH, None, f).left.map(_ -> f))
      }
      .fold({ case (err, file) => sys.error(s"Could not load package $file: $err") }, identity)
  }
}

object SandboxServer {
  private val logger = LoggerFactory.getLogger(classOf[SandboxServer])

  private val defaultActorSystemName = "sandbox"

  case class ApiServerState(
      ledgerId: LedgerId,
      apiServer: ApiServer,
      indexAndWriteService: IndexAndWriteService,
  ) extends AutoCloseable {
    def port: Int = apiServer.port

    override def close(): Unit = {
      logger.debug("Tearing down the API server...")
      apiServer.close()
      logger.debug("Tearing down the index and write service...")
      indexAndWriteService.close()
    }
  }

  case class Infrastructure(actorSystemName: String) extends AutoCloseable {
    val actorSystem: ActorSystem = ActorSystem(actorSystemName)
    val materializer: ActorMaterializer = ActorMaterializer()(actorSystem)
    val metricsManager: MetricsManager = MetricsManager()

    def executionContext: ExecutionContext = materializer.executionContext

    override def close(): Unit = {
      materializer.shutdown()
      Await.result(actorSystem.terminate(), Async.tolerance)
      metricsManager.close()
    }
  }
}
