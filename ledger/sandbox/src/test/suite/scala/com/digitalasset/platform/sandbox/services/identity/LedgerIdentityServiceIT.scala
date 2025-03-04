// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox.services.identity

import java.util.UUID

import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.ledger.api.domain.LedgerId
import com.digitalasset.ledger.api.testing.utils.SuiteResourceManagementAroundEach
import com.digitalasset.platform.common.LedgerIdMode
import com.digitalasset.platform.sandbox.config.SandboxConfig
import com.digitalasset.platform.sandbox.persistence.{PostgresAroundAll, PostgresAroundEach}
import com.digitalasset.platform.sandbox.services.SandboxFixture
import org.scalatest.{Matchers, WordSpec}
import scalaz.syntax.tag._

sealed trait LedgerIdentityServiceITBaseGiven
    extends WordSpec
    with Matchers
    with SandboxFixture
    with SuiteResourceManagementAroundEach {

  private lazy val givenLedgerId: String = UUID.randomUUID.toString

  override protected def config: SandboxConfig =
    super.config.copy(ledgerIdMode =
      LedgerIdMode.Static(LedgerId(Ref.LedgerString.assertFromString(givenLedgerId))))

  // This test relies on inheriting from SuiteResourceManagementAroundEach to restart the ledger across test cases

  "A platform" when {
    "started" should {
      "expose the expected ledger identifier" in {
        ledgerIdOnServer.unwrap shouldEqual givenLedgerId
      }
      "expose the expected ledger identifier across restarts" in {
        ledgerIdOnServer.unwrap shouldEqual givenLedgerId
      }
    }
  }

}

final class LedgerIdentityServiceInMemoryGivenIT extends LedgerIdentityServiceITBaseGiven

final class LedgerIdentityServicePostgresGivenIT
    extends LedgerIdentityServiceITBaseGiven
    with PostgresAroundAll {

  override protected def config: SandboxConfig =
    super.config.copy(jdbcUrl = Some(postgresFixture.jdbcUrl))

}

sealed trait LedgerIdentityServiceITBaseDynamic
    extends WordSpec
    with Matchers
    with SandboxFixture
    with SuiteResourceManagementAroundEach {

  override protected def config: SandboxConfig =
    super.config.copy(ledgerIdMode = LedgerIdMode.Dynamic())

  @volatile private var firstRunLedgerId: String = _

  // This test relies on inheriting from SuiteResourceManagementAroundEach to restart the ledger across test cases

  "A platform" when {

    "started" should {

      "expose a ledger identifer" in {
        firstRunLedgerId = ledgerIdOnServer.unwrap
        firstRunLedgerId should not be empty
      }

      "have different identifiers across restarts" in {
        firstRunLedgerId should not equal ledgerIdOnServer.unwrap
      }

    }

  }

}

final class LedgerIdentityServiceInMemoryDynamicIT extends LedgerIdentityServiceITBaseDynamic

final class LedgerIdentityServicePostgresDynamicIT
    extends LedgerIdentityServiceITBaseDynamic
    with PostgresAroundEach {

  override protected def config: SandboxConfig =
    super.config.copy(jdbcUrl = Some(postgresFixture.jdbcUrl))

}

final class LedgerIdentityServicePostgresDynamicSharedPostgresIT
    extends WordSpec
    with Matchers
    with SandboxFixture
    with SuiteResourceManagementAroundEach
    with PostgresAroundAll {

  override protected def config: SandboxConfig =
    super.config
      .copy(
        jdbcUrl = Some(postgresFixture.jdbcUrl),
        ledgerIdMode = Option(firstRunLedgerId).fold[LedgerIdMode](LedgerIdMode.Dynamic())(id =>
          LedgerIdMode.Static(LedgerId(Ref.LedgerString.assertFromString(id))))
      )

  @volatile private var firstRunLedgerId: String = _

  // This test relies on inheriting from SuiteResourceManagementAroundEach to restart the ledger across test cases AND
  // on PostgresAroundAll to share the Postgres instance across restarts to test the peculiar semantics of this case

  "A platform" when {

    "started" should {

      "expose a ledger identifer" in {
        firstRunLedgerId = ledgerIdOnServer.unwrap
        firstRunLedgerId should not be empty
      }

      "have the assigned random ledger identifier after a restart" in {
        firstRunLedgerId shouldEqual ledgerIdOnServer.unwrap
      }

    }

  }

}
