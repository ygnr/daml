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
import com.digitalasset.platform.sandbox.stores.InMemoryPackageStore

import scala.concurrent.duration.DurationInt
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
      // fully tear down the old server
      runningServer.get.close()

      // TODO: eliminate the state mutation somehow
      // yes, it's horrible that we mutate the state here, but believe me, it's
      // still an improvement to what we had before!
      runningServer = Some(
        RunningSandboxServer.recreate(
          config,
          infrastructure,
          packageStore,
          runningServer.get.port,
          () => resetAndRestartServer(),
        ))
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
  private val asyncTolerance = 30.seconds

  private val defaultActorSystemName = "sandbox"

  case class ApiServerState(
      ledgerId: LedgerId,
      apiServer: ApiServer,
      indexAndWriteService: AutoCloseable,
  ) extends AutoCloseable {
    def port: Int = apiServer.port

    override def close(): Unit = {
      apiServer.close() //fully tear down the old server.
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
      Await.result(actorSystem.terminate(), asyncTolerance)
      metricsManager.close()
    }
  }
}
