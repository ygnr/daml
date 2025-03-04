// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
package com.digitalasset.platform.sandbox.stores.ledger.sql.dao

import java.io.InputStream
import java.sql.Connection
import java.util.Date

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import anorm.SqlParser._
import anorm.ToStatement.optionToStatement
import anorm.{AkkaStream, BatchSql, Macro, NamedParameter, RowParser, SQL, SqlParser}
import com.daml.ledger.participant.state.index.v2.PackageDetails
import com.daml.ledger.participant.state.v1.{AbsoluteContractInst, TransactionId}
import com.digitalasset.daml.lf.archive.Decode
import com.digitalasset.daml.lf.data.Ref.{
  ContractIdString,
  LedgerString,
  PackageId,
  Party,
  TransactionIdString
}
import com.digitalasset.daml.lf.data.Relation.Relation
import com.digitalasset.daml.lf.transaction.Node
import com.digitalasset.daml.lf.transaction.Node.{GlobalKey, KeyWithMaintainers}
import com.digitalasset.daml.lf.value.Value
import com.digitalasset.daml.lf.value.Value.AbsoluteContractId
import com.digitalasset.daml_lf_dev.DamlLf.Archive
import com.digitalasset.ledger._
import com.digitalasset.ledger.api.domain.RejectionReason._
import com.digitalasset.ledger.api.domain.{LedgerId, PartyDetails, RejectionReason}
import com.digitalasset.platform.common.logging.NamedLoggerFactory
import com.digitalasset.platform.common.util.DirectExecutionContext
import com.digitalasset.platform.participant.util.EventFilter.TemplateAwareFilter
import com.digitalasset.platform.sandbox.stores.ActiveLedgerState.{
  ActiveContract,
  Contract,
  DivulgedContract
}
import com.digitalasset.platform.sandbox.stores._
import com.digitalasset.platform.sandbox.stores.ledger.LedgerEntry
import com.digitalasset.platform.sandbox.stores.ledger.LedgerEntry._
import com.digitalasset.platform.sandbox.stores.ledger.sql.dao.JdbcLedgerDao.{
  H2DatabaseQueries,
  PostgresQueries
}
import com.digitalasset.platform.sandbox.stores.ledger.sql.serialisation.{
  ContractSerializer,
  KeyHasher,
  TransactionSerializer,
  ValueSerializer
}
import com.digitalasset.platform.sandbox.stores.ledger.sql.util.Conversions._
import com.digitalasset.platform.sandbox.stores.ledger.sql.util.DbDispatcher
import com.google.common.io.ByteStreams
import scalaz.syntax.tag._

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

private class JdbcLedgerDao(
    dbDispatcher: DbDispatcher,
    contractSerializer: ContractSerializer,
    transactionSerializer: TransactionSerializer,
    valueSerializer: ValueSerializer,
    keyHasher: KeyHasher,
    dbType: DbType,
    loggerFactory: NamedLoggerFactory,
    executionContext: ExecutionContext)
    extends LedgerDao {

  private val queries = dbType match {
    case DbType.Postgres => PostgresQueries
    case DbType.H2Database => H2DatabaseQueries
  }
  private val logger = loggerFactory.getLogger(getClass)

  private val SQL_SELECT_LEDGER_ID = SQL("select ledger_id from parameters")

  override def lookupLedgerId(): Future[Option[LedgerId]] =
    dbDispatcher
      .executeSql("get_ledger_id") { implicit conn =>
        SQL_SELECT_LEDGER_ID
          .as(ledgerString("ledger_id").map(id => LedgerId(id.toString)).singleOpt)
      }

  private val SQL_SELECT_LEDGER_END = SQL("select ledger_end from parameters")

  override def lookupLedgerEnd(): Future[Long] =
    dbDispatcher.executeSql("get_ledger_end") { implicit conn =>
      SQL_SELECT_LEDGER_END
        .as(long("ledger_end").single)
    }

  private val SQL_SELECT_EXTERNAL_LEDGER_END = SQL("select external_ledger_end from parameters")

  override def lookupExternalLedgerEnd(): Future[Option[LedgerString]] =
    dbDispatcher.executeSql("get_external_ledger_end") { implicit conn =>
      SQL_SELECT_EXTERNAL_LEDGER_END
        .as(ledgerString("external_ledger_end").?.single)
    }

  private val SQL_INITIALIZE = SQL(
    "insert into parameters(ledger_id, ledger_end) VALUES({LedgerId}, {LedgerEnd})")

  override def initializeLedger(ledgerId: LedgerId, ledgerEnd: LedgerOffset): Future[Unit] =
    dbDispatcher.executeSql("initialize_ledger_parameters") { implicit conn =>
      val _ = SQL_INITIALIZE
        .on("LedgerId" -> ledgerId.unwrap)
        .on("LedgerEnd" -> ledgerEnd)
        .execute()
      ()
    }

  // Note that the ledger entries grow monotonically, however we store many ledger entries in parallel,
  // and thus we need to make sure to only update the ledger end when the ledger entry we're committing
  // is advancing it.
  private val SQL_UPDATE_LEDGER_END = SQL(
    "update parameters set ledger_end = {LedgerEnd}, external_ledger_end = {ExternalLedgerEnd} where ledger_end < {LedgerEnd}")

  private def updateLedgerEnd(ledgerEnd: LedgerOffset, externalLedgerEnd: Option[LedgerString])(
      implicit conn: Connection): Unit = {
    SQL_UPDATE_LEDGER_END
      .on("LedgerEnd" -> ledgerEnd, "ExternalLedgerEnd" -> externalLedgerEnd)
      .execute()
    ()
  }

  private val SQL_UPDATE_EXTERNAL_LEDGER_END = SQL(
    "update parameters set external_ledger_end = {ExternalLedgerEnd}")

  private def updateExternalLedgerEnd(externalLedgerEnd: LedgerString)(
      implicit conn: Connection): Unit = {
    SQL_UPDATE_EXTERNAL_LEDGER_END
      .on("ExternalLedgerEnd" -> externalLedgerEnd)
      .execute()
    ()
  }

  private val SQL_INSERT_CONTRACT_KEY =
    SQL(
      "insert into contract_keys(package_id, name, value_hash, contract_id) values({package_id}, {name}, {value_hash}, {contract_id})")

  private val SQL_SELECT_CONTRACT_KEY =
    SQL(
      "select contract_id from contract_keys where package_id={package_id} and name={name} and value_hash={value_hash}")

  private val SQL_SELECT_CONTRACT_KEY_FOR_PARTY =
    SQL(
      """select ck.contract_id from contract_keys ck
        |left join contract_witnesses cowi on ck.contract_id = cowi.contract_id and cowi.witness = {party}
        |left join contract_divulgences codi on ck.contract_id = codi.contract_id and codi.party = {party}
        |where
        |  ck.package_id={package_id} and
        |  ck.name={name} and
        |  ck.value_hash={value_hash} and
        |  (cowi.witness is not null or codi.party is not null)
        |""".stripMargin)

  private val SQL_REMOVE_CONTRACT_KEY =
    SQL("delete from contract_keys where contract_id={contract_id}")

  private[this] def storeContractKey(key: GlobalKey, cid: AbsoluteContractId)(
      implicit connection: Connection): Boolean =
    SQL_INSERT_CONTRACT_KEY
      .on(
        "package_id" -> key.templateId.packageId,
        "name" -> key.templateId.qualifiedName.toString,
        "value_hash" -> keyHasher.hashKeyString(key),
        "contract_id" -> cid.coid
      )
      .execute()

  private[this] def removeContractKey(cid: AbsoluteContractId)(
      implicit connection: Connection): Boolean =
    SQL_REMOVE_CONTRACT_KEY
      .on(
        "contract_id" -> cid.coid,
      )
      .execute()

  private[this] def selectContractKey(key: GlobalKey)(
      implicit connection: Connection): Option[AbsoluteContractId] =
    SQL_SELECT_CONTRACT_KEY
      .on(
        "package_id" -> key.templateId.packageId,
        "name" -> key.templateId.qualifiedName.toString,
        "value_hash" -> keyHasher.hashKeyString(key)
      )
      .as(ledgerString("contract_id").singleOpt)
      .map(AbsoluteContractId)

  override def lookupKey(key: Node.GlobalKey, forParty: Party): Future[Option[AbsoluteContractId]] =
    dbDispatcher.executeSql("lookup_contract_by_key")(
      implicit conn =>
        SQL_SELECT_CONTRACT_KEY_FOR_PARTY
          .on(
            "package_id" -> key.templateId.packageId,
            "name" -> key.templateId.qualifiedName.toString,
            "value_hash" -> keyHasher.hashKeyString(key),
            "party" -> forParty
          )
          .as(ledgerString("contract_id").singleOpt)
          .map(AbsoluteContractId))

  private def storeContract(offset: Long, contract: ActiveContract)(
      implicit connection: Connection): Unit = storeContracts(offset, List(contract))

  private def archiveContract(offset: Long, cid: AbsoluteContractId)(
      implicit connection: Connection): Boolean =
    SQL_ARCHIVE_CONTRACT
      .on(
        "id" -> cid.coid,
        "archive_offset" -> offset
      )
      .execute()

  private val SQL_INSERT_CONTRACT =
    """insert into contracts(id, transaction_id, create_event_id, workflow_id, package_id, name, create_offset, key)
      |values({id}, {transaction_id}, {create_event_id}, {workflow_id}, {package_id}, {name}, {create_offset}, {key})""".stripMargin

  private val SQL_INSERT_CONTRACT_WITNESS =
    "insert into contract_witnesses(contract_id, witness) values({contract_id}, {witness})"

  private val SQL_INSERT_CONTRACT_KEY_MAINTAINERS =
    "insert into contract_key_maintainers(contract_id, maintainer) values({contract_id}, {maintainer})"

  private val SQL_INSERT_CONTRACT_SIGNATORIES =
    "insert into contract_signatories(contract_id, signatory) values({contract_id}, {signatory})"

  private val SQL_INSERT_CONTRACT_OBSERVERS =
    "insert into contract_observers(contract_id, observer) values({contract_id}, {observer})"

  private def storeContractData(contracts: Seq[(AbsoluteContractId, AbsoluteContractInst)])(
      implicit connection: Connection): Unit = {
    val namedContractDataParams = contracts
      .map {
        case (cid, contract) =>
          Seq[NamedParameter](
            "id" -> cid.coid,
            "contract" -> contractSerializer
              .serializeContractInstance(contract)
              .getOrElse(sys.error(s"failed to serialize contract! cid:${cid.coid}"))
          )
      }

    if (namedContractDataParams.nonEmpty) {
      val _ = executeBatchSql(
        queries.SQL_INSERT_CONTRACT_DATA,
        namedContractDataParams
      )
    }
  }

  private def storeContracts(offset: Long, contracts: immutable.Seq[ActiveContract])(
      implicit connection: Connection): Unit = {

    // An ACS contract contains several collections (e.g., witnesses or divulgences).
    // The contract is therefore stored in several SQL tables.

    // Part 1: insert the contract data into the 'contracts' table
    if (contracts.nonEmpty) {
      val namedContractParams = contracts
        .map(
          c =>
            Seq[NamedParameter](
              "id" -> c.id.coid,
              "transaction_id" -> c.transactionId,
              "create_event_id" -> c.eventId,
              "workflow_id" -> c.workflowId,
              "package_id" -> c.contract.template.packageId,
              "name" -> c.contract.template.qualifiedName.toString,
              "create_offset" -> offset,
              "key" -> c.key
                .map(
                  k =>
                    valueSerializer
                      .serializeValue(k.key)
                      .getOrElse(
                        sys.error(s"failed to serialize contract key value! cid:${c.id.coid}")))
          )
        )

      executeBatchSql(
        SQL_INSERT_CONTRACT,
        namedContractParams
      )

      storeContractData(contracts.map(c => (c.id, c.contract)))

      // Part 2: insert witnesses into the 'contract_witnesses' table
      val namedWitnessesParams = contracts
        .flatMap(
          c =>
            c.witnesses.map(
              w =>
                Seq[NamedParameter](
                  "contract_id" -> c.id.coid,
                  "witness" -> w
              ))
        )
        .toArray

      if (!namedWitnessesParams.isEmpty) {
        executeBatchSql(
          SQL_INSERT_CONTRACT_WITNESS,
          namedWitnessesParams
        )
      }

      // Part 3: insert divulgences into the 'contract_divulgences' table
      val hasNonLocalDivulgence =
        contracts.exists(c => c.divulgences.exists(d => d._2 != c.transactionId))
      if (hasNonLocalDivulgence) {
        // There is at least one contract that was divulged to some party after it was commited.
        // This happens when writing contracts produced by the scenario loader.
        // Since we only have the transaction IDs when the contract was divulged, we need to look up the corresponding
        // ledger offsets.
        val namedDivulgenceParams = contracts
          .flatMap(
            c =>
              c.divulgences.map(
                w =>
                  Seq[NamedParameter](
                    "contract_id" -> c.id.coid,
                    "party" -> w._1,
                    "transaction_id" -> w._2
                ))
          )
          .toArray

        if (!namedDivulgenceParams.isEmpty) {
          executeBatchSql(
            queries.SQL_BATCH_INSERT_DIVULGENCES_FROM_TRANSACTION_ID,
            namedDivulgenceParams
          )
        }
      } else {
        val namedDivulgenceParams = contracts
          .flatMap(
            c =>
              c.divulgences.map(
                w =>
                  Seq[NamedParameter](
                    "contract_id" -> c.id.coid,
                    "party" -> w._1,
                    "ledger_offset" -> offset,
                    "transaction_id" -> c.transactionId
                ))
          )
          .toArray

        if (!namedDivulgenceParams.isEmpty) {
          executeBatchSql(
            queries.SQL_BATCH_INSERT_DIVULGENCES,
            namedDivulgenceParams
          )
        }
      }

      // Part 4: insert key maintainers into the 'contract_key_maintainers' table
      val namedKeyMaintainerParams = contracts
        .flatMap(
          c =>
            c.key
              .map(
                k =>
                  k.maintainers.map(
                    p =>
                      Seq[NamedParameter](
                        "contract_id" -> c.id.coid,
                        "maintainer" -> p
                    )))
              .getOrElse(Set.empty)
        )
        .toArray

      if (!namedKeyMaintainerParams.isEmpty) {
        executeBatchSql(
          SQL_INSERT_CONTRACT_KEY_MAINTAINERS,
          namedKeyMaintainerParams
        )
      }
    }

    // Part 5: insert signatories into the 'contract_signatories' table
    val namedSignatoriesParams = contracts
      .flatMap(
        c =>
          c.signatories.map(
            w =>
              Seq[NamedParameter](
                "contract_id" -> c.id.coid,
                "signatory" -> w
            ))
      )
      .toArray

    if (!namedSignatoriesParams.isEmpty) {
      executeBatchSql(
        SQL_INSERT_CONTRACT_SIGNATORIES,
        namedSignatoriesParams
      )
    }

    // Part 6: insert observers into the 'contract_observers' table
    val namedObserversParams = contracts
      .flatMap(
        c =>
          c.observers.map(
            w =>
              Seq[NamedParameter](
                "contract_id" -> c.id.coid,
                "observer" -> w
            ))
      )
      .toArray

    if (!namedObserversParams.isEmpty) {
      executeBatchSql(
        SQL_INSERT_CONTRACT_OBSERVERS,
        namedObserversParams
      )
    }
    ()
  }

  private val SQL_ARCHIVE_CONTRACT =
    SQL("""update contracts set archive_offset = {archive_offset} where id = {id}""")

  private val SQL_INSERT_TRANSACTION =
    SQL(
      """insert into ledger_entries(typ, ledger_offset, transaction_id, command_id, application_id, submitter, workflow_id, effective_at, recorded_at, transaction)
        |values('transaction', {ledger_offset}, {transaction_id}, {command_id}, {application_id}, {submitter}, {workflow_id}, {effective_at}, {recorded_at}, {transaction})""".stripMargin)

  private val SQL_INSERT_REJECTION =
    SQL(
      """insert into ledger_entries(typ, ledger_offset, command_id, application_id, submitter, recorded_at, rejection_type, rejection_description)
        |values('rejection', {ledger_offset}, {command_id}, {application_id}, {submitter}, {recorded_at}, {rejection_type}, {rejection_description})""".stripMargin)

  private val SQL_BATCH_INSERT_DISCLOSURES =
    "insert into disclosures(transaction_id, event_id, party) values({transaction_id}, {event_id}, {party})"

  private val SQL_INSERT_CHECKPOINT =
    SQL(
      "insert into ledger_entries(typ, ledger_offset, recorded_at) values('checkpoint', {ledger_offset}, {recorded_at})")

  /**
    * Updates the active contract set from the given DAML transaction.
    * Note: This involves checking the validity of the given DAML transaction.
    * Invalid transactions trigger a rollback of the current SQL transaction.
    */
  private def updateActiveContractSet(
      offset: Long,
      tx: Transaction,
      localDivulgence: Relation[EventId, Party],
      globalDivulgence: Relation[AbsoluteContractId, Party],
      divulgedContracts: List[(Value.AbsoluteContractId, AbsoluteContractInst)])(
      implicit connection: Connection): Option[RejectionReason] = tx match {
    case Transaction(
        _,
        transactionId,
        _,
        _,
        workflowId,
        ledgerEffectiveTime,
        _,
        transaction,
        disclosure) =>
      final class AcsStoreAcc extends ActiveLedgerState[AcsStoreAcc] {

        override def lookupContractLet(cid: AbsoluteContractId): Option[LetLookup] =
          lookupContractLetSync(cid)

        override def keyExists(key: GlobalKey): Boolean = selectContractKey(key).isDefined

        override def addContract(c: ActiveContract, keyO: Option[GlobalKey]): AcsStoreAcc = {
          storeContract(offset, c)
          keyO.foreach(key => storeContractKey(key, c.id))
          this
        }

        override def removeContract(cid: AbsoluteContractId): AcsStoreAcc = {
          archiveContract(offset, cid)
          removeContractKey(cid)
          this
        }

        override def addParties(parties: Set[Party]): AcsStoreAcc = {
          val partyParams = parties.toList.map(
            p =>
              Seq[NamedParameter](
                "name" -> (p: String),
                "ledger_offset" -> offset,
                "explicit" -> false
            ))
          if (partyParams.nonEmpty) {
            executeBatchSql(queries.SQL_IMPLICITLY_INSERT_PARTIES, partyParams)
          }
          this
        }

        override def divulgeAlreadyCommittedContracts(
            transactionId: TransactionIdString,
            global: Relation[AbsoluteContractId, Party],
            divulgedContracts: List[(Value.AbsoluteContractId, AbsoluteContractInst)])
          : AcsStoreAcc = {
          val divulgenceParams = global
            .flatMap {
              case (cid, parties) =>
                parties.map(
                  p =>
                    Seq[NamedParameter](
                      "contract_id" -> cid.coid,
                      "party" -> p,
                      "ledger_offset" -> offset,
                      "transaction_id" -> transactionId
                  ))
            }
          // Note: the in-memory ledger only stores divulgence for contracts in the ACS.
          // Do we need here the equivalent to 'contracts.intersectWith(global)', used in the in-memory
          // implementation of implicitlyDisclose?
          if (divulgenceParams.nonEmpty) {
            executeBatchSql(queries.SQL_BATCH_INSERT_DIVULGENCES, divulgenceParams)
          }
          this
        }
      }

      // this should be a class member field, we can't move it out yet as the functions above are closing over to the implicit Connection
      val acsManager = new ActiveLedgerStateManager(new AcsStoreAcc)

      // Note: ACS is typed as Unit here, as the ACS is given implicitly by the current database state
      // within the current SQL transaction. All of the given functions perform side effects to update the database.
      val atr = acsManager.addTransaction(
        ledgerEffectiveTime,
        transactionId,
        workflowId,
        transaction,
        disclosure,
        localDivulgence,
        globalDivulgence,
        divulgedContracts
      )

      atr match {
        case Left(err) =>
          Some(RejectionReason.Inconsistent(s"Reason: ${err.mkString("[", ", ", "]")}"))
        case Right(_) => None
      }
  }

  private def storeTransaction(offset: Long, tx: LedgerEntry.Transaction, txBytes: Array[Byte])(
      implicit connection: Connection): Unit = {
    SQL_INSERT_TRANSACTION
      .on(
        "ledger_offset" -> offset,
        "transaction_id" -> tx.transactionId,
        "command_id" -> (tx.commandId: Option[String]),
        "application_id" -> (tx.applicationId: Option[String]),
        "submitter" -> (tx.submittingParty: Option[String]),
        "workflow_id" -> tx.workflowId.getOrElse(""),
        "effective_at" -> tx.ledgerEffectiveTime,
        "recorded_at" -> tx.recordedAt,
        "transaction" -> txBytes
      )
      .execute()

    val disclosureParams = tx.explicitDisclosure.flatMap {
      case (eventId, parties) =>
        parties.map(
          p =>
            Seq[NamedParameter](
              "transaction_id" -> tx.transactionId,
              "event_id" -> eventId,
              "party" -> p
          ))
    }
    if (disclosureParams.nonEmpty) {
      executeBatchSql(
        SQL_BATCH_INSERT_DISCLOSURES,
        disclosureParams
      )
    }

    ()
  }

  private def storeRejection(offset: Long, rejection: LedgerEntry.Rejection)(
      implicit connection: Connection): Unit = {
    val (rejectionDescription, rejectionType) = writeRejectionReason(rejection.rejectionReason)
    SQL_INSERT_REJECTION
      .on(
        "ledger_offset" -> offset,
        "command_id" -> rejection.commandId,
        "application_id" -> rejection.applicationId,
        "submitter" -> rejection.submitter,
        "recorded_at" -> rejection.recordTime,
        "rejection_description" -> rejectionDescription,
        "rejection_type" -> rejectionType
      )
      .execute()

    ()
  }

  private def storeCheckpoint(offset: Long, checkpoint: LedgerEntry.Checkpoint)(
      implicit connection: Connection): Unit = {
    SQL_INSERT_CHECKPOINT
      .on("ledger_offset" -> offset, "recorded_at" -> checkpoint.recordedAt)
      .execute()

    ()
  }

  private def serializeTransaction(ledgerEntry: LedgerEntry): Array[Byte] = {
    ledgerEntry match {
      case Transaction(transactionId, _, _, _, _, _, _, genTransaction, _) =>
        transactionSerializer
          .serializeTransaction(genTransaction)
          .fold(
            err =>
              sys.error(
                s"Failed to serialize transaction! trId: $transactionId. Details: ${err.errorMessage}."),
            identity
          )
      case _: LedgerEntry.Rejection | _: LedgerEntry.Checkpoint => Array.empty[Byte]
    }
  }

  //TODO: test it for failures..
  override def storeLedgerEntry(
      offset: Long,
      newLedgerEnd: Long,
      externalOffset: Option[ExternalOffset],
      ledgerEntry: PersistenceEntry): Future[PersistenceResponse] = {
    import PersistenceResponse._

    val txBytes = serializeTransaction(ledgerEntry.entry)

    def insertEntry(le: PersistenceEntry)(implicit conn: Connection): PersistenceResponse =
      le match {
        case PersistenceEntry.Transaction(
            tx,
            localDivulgence,
            globalDivulgence,
            divulgedContracts) =>
          Try {
            storeTransaction(offset, tx, txBytes)

            // Ensure divulged contracts are known about before they are referred to.
            storeContractData(divulgedContracts)

            updateActiveContractSet(
              offset,
              tx,
              localDivulgence,
              globalDivulgence,
              divulgedContracts)
              .flatMap { rejectionReason =>
                // we need to rollback the existing sql transaction
                conn.rollback()
                // we only need to store a rejection if the full submitter information is available.
                for {
                  cmdId <- tx.commandId
                  appId <- tx.applicationId
                  submitter <- tx.submittingParty
                } yield {
                  insertEntry(
                    PersistenceEntry.Rejection(
                      Rejection(
                        tx.recordedAt,
                        cmdId,
                        appId,
                        submitter,
                        rejectionReason
                      )))

                }
              } getOrElse Ok
          }.recover {
            case NonFatal(e) if e.getMessage.contains(queries.DUPLICATE_KEY_ERROR) =>
              logger.warn(
                "Ignoring duplicate submission for submitter{}, applicationId {}, commandId {}",
                tx.submittingParty,
                tx.applicationId,
                tx.commandId)
              conn.rollback()
              Duplicate
          }.get

        case PersistenceEntry.Rejection(rejection) =>
          storeRejection(offset, rejection)
          Ok

        case PersistenceEntry.Checkpoint(checkpoint) =>
          storeCheckpoint(offset, checkpoint)
          Ok
      }

    dbDispatcher
      .executeSql("store_ledger_entry", Some(ledgerEntry.getClass.getSimpleName)) { implicit conn =>
        val resp = insertEntry(ledgerEntry)
        updateLedgerEnd(newLedgerEnd, externalOffset)
        resp
      }
  }

  override def storeInitialState(
      activeContracts: immutable.Seq[ActiveContract],
      ledgerEntries: immutable.Seq[(LedgerOffset, LedgerEntry)],
      newLedgerEnd: LedgerOffset
  ): Future[Unit] = {
    // A map to look up offset by transaction ID
    // Needed to store contracts: in the database, we store the offset at which a contract was created,
    // the Contract object stores the transaction ID at which it was created.
    val transactionIdMap = ledgerEntries.collect {
      case (i, tx: LedgerEntry.Transaction) => tx.transactionId -> i
    }.toMap

    val transactionBytes = ledgerEntries.collect {
      case (offset, entry: LedgerEntry.Transaction) => offset -> serializeTransaction(entry)
    }.toMap

    dbDispatcher
      .executeSql(
        "store_initial_state_from_scenario",
        Some(s"active contracts: ${activeContracts.size}, ledger entries: ${ledgerEntries.size}")) {
        implicit conn =>
          // First, store all ledger entries without updating the ACS
          // We can't use the storeLedgerEntry(), as that one does update the ACS
          ledgerEntries.foreach {
            case (i, le) =>
              le match {
                case tx: LedgerEntry.Transaction => storeTransaction(i, tx, transactionBytes(i))
                case rj: LedgerEntry.Rejection => storeRejection(i, rj)
                case cp: LedgerEntry.Checkpoint => storeCheckpoint(i, cp)
              }
          }

          // Then, write the given ACS. We trust the caller to supply an ACS that is
          // consistent with the given list of ledger entries.
          activeContracts.foreach(c => storeContract(transactionIdMap(c.transactionId), c))

          updateLedgerEnd(newLedgerEnd, None)
      }
  }

  private def writeRejectionReason(rejectionReason: RejectionReason) =
    (rejectionReason.description, rejectionReason match {
      case _: Inconsistent => "Inconsistent"
      case _: OutOfQuota => "OutOfQuota"
      case _: TimedOut => "TimedOut"
      case _: Disputed => "Disputed"
      case _: DuplicateCommandId => "DuplicateCommandId"
      case _: PartyNotKnownOnLedger => "PartyNotKnownOnLedger"
      case _: SubmitterCannotActViaParticipant => "SubmitterCannotActViaParticipant"
    })

  private def readRejectionReason(rejectionType: String, description: String): RejectionReason =
    rejectionType match {
      case "Inconsistent" => Inconsistent(description)
      case "OutOfQuota" => OutOfQuota(description)
      case "TimedOut" => TimedOut(description)
      case "Disputed" => Disputed(description)
      case "DuplicateCommandId" => DuplicateCommandId(description)
      case "PartyNotKnownOnLedger" => PartyNotKnownOnLedger(description)
      case "SubmitterCannotActViaParticipant" => SubmitterCannotActViaParticipant(description)
      case typ => sys.error(s"unknown rejection reason: $typ")
    }

  private val SQL_SELECT_ENTRY =
    SQL("select * from ledger_entries where ledger_offset={ledger_offset}")

  private val SQL_SELECT_TRANSACTION =
    SQL("select * from ledger_entries where transaction_id={transaction_id}")

  private val SQL_SELECT_DISCLOSURE =
    SQL("select * from disclosures where transaction_id={transaction_id}")

  case class ParsedEntry(
      typ: String,
      transactionId: Option[TransactionIdString],
      commandId: Option[CommandId],
      applicationId: Option[ApplicationId],
      submitter: Option[Party],
      workflowId: Option[WorkflowId],
      effectiveAt: Option[Date],
      recordedAt: Option[Date],
      transaction: Option[Array[Byte]],
      rejectionType: Option[String],
      rejectionDesc: Option[String],
      offset: Long)

  private val EntryParser: RowParser[ParsedEntry] = (
    str("typ") ~
      ledgerString("transaction_id").? ~
      ledgerString("command_id").? ~
      ledgerString("application_id").? ~
      party("submitter").? ~
      ledgerString("workflow_id")(emptyStringToNullColumn).? ~
      date("effective_at").? ~
      date("recorded_at").? ~
      byteArray("transaction").? ~
      str("rejection_type").? ~
      str("rejection_description").? ~
      long("ledger_offset")
  ) map flatten map (ParsedEntry.tupled)

  private val DisclosureParser = ledgerString("event_id") ~ party("party") map flatten

  private def toLedgerEntry(
      parsedEntry: ParsedEntry,
      disclosureOpt: Option[Relation[EventId, Party]]): (Long, LedgerEntry) = parsedEntry match {
    case ParsedEntry(
        "transaction",
        Some(transactionId),
        commandId,
        applicationId,
        submitter,
        workflowId,
        Some(effectiveAt),
        Some(recordedAt),
        Some(transactionStream),
        None,
        None,
        offset) =>
      offset -> LedgerEntry.Transaction(
        commandId,
        transactionId,
        applicationId,
        submitter,
        workflowId,
        effectiveAt.toInstant,
        recordedAt.toInstant,
        transactionSerializer
          .deserializeTransaction(transactionStream)
          .fold(
            err =>
              sys.error(s"failed to deserialize transaction! trId: $transactionId: error: $err"),
            identity),
        disclosureOpt.getOrElse(Map.empty)
      )
    case ParsedEntry(
        "rejection",
        None,
        Some(commandId),
        Some(applicationId),
        Some(submitter),
        None,
        None,
        Some(recordedAt),
        None,
        Some(rejectionType),
        Some(rejectionDescription),
        offset) =>
      val rejectionReason = readRejectionReason(rejectionType, rejectionDescription)
      offset -> LedgerEntry
        .Rejection(recordedAt.toInstant, commandId, applicationId, submitter, rejectionReason)
    case ParsedEntry(
        "checkpoint",
        None,
        None,
        None,
        None,
        None,
        None,
        Some(recordedAt),
        None,
        None,
        None,
        offset) =>
      offset -> LedgerEntry.Checkpoint(recordedAt.toInstant)
    case invalidRow =>
      sys.error(s"invalid ledger entry for offset: ${invalidRow.offset}. database row: $invalidRow")
  }

  override def lookupLedgerEntry(offset: Long): Future[Option[LedgerEntry]] = {
    dbDispatcher
      .executeSql("lookup_ledger_entry_at_offset", Some(s"offset: $offset")) { implicit conn =>
        val entry = SQL_SELECT_ENTRY
          .on("ledger_offset" -> offset)
          .as(EntryParser.singleOpt)
        entry.map(e => e -> loadDisclosureOptForEntry(e))
      }
      .map(_.map((toLedgerEntry _).tupled(_)._2))(executionContext)
  }

  override def lookupTransaction(
      transactionId: TransactionId): Future[Option[(LedgerOffset, LedgerEntry.Transaction)]] = {
    dbDispatcher
      .executeSql("lookup_transaction", Some(s"tx id: $transactionId")) { implicit conn =>
        val entry = SQL_SELECT_TRANSACTION
          .on("transaction_id" -> (transactionId: String))
          .as(EntryParser.singleOpt)
        entry.map(e => e -> loadDisclosureOptForEntry(e))
      }
      .map(_.map((toLedgerEntry _).tupled)
        .collect {
          case (offset, t: LedgerEntry.Transaction) => offset -> t
        })(executionContext)
  }

  private val ContractDataParser = (ledgerString("id")
    ~ ledgerString("transaction_id").?
    ~ ledgerString("create_event_id").?
    ~ ledgerString("workflow_id").?
    ~ date("effective_at").?
    ~ binaryStream("contract")
    ~ binaryStream("key").?
    ~ str("signatories").?
    ~ str("observers").? map flatten)

  private val ContractLetParser = (ledgerString("id")
    ~ date("effective_at").? map flatten)

  private val SQL_SELECT_CONTRACT =
    SQL(queries.SQL_SELECT_CONTRACT)

  private val SQL_SELECT_CONTRACT_LET =
    SQL("""
        |select c.id, le.effective_at
        |from contracts c
        |left join ledger_entries le on c.transaction_id = le.transaction_id
        |where c.id={contract_id} and c.archive_offset is null""".stripMargin)

  private val SQL_SELECT_WITNESS =
    SQL("select witness from contract_witnesses where contract_id={contract_id}")

  private val DivulgenceParser = (party("party")
    ~ long("ledger_offset")
    ~ ledgerString("transaction_id") map flatten)

  private val SQL_SELECT_DIVULGENCE =
    SQL(
      "select party, ledger_offset, transaction_id from contract_divulgences where contract_id={contract_id}")

  private val SQL_SELECT_KEY_MAINTAINERS =
    SQL("select maintainer from contract_key_maintainers where contract_id={contract_id}")

  private def lookupContractSync(contractId: AbsoluteContractId, forParty: Party)(
      implicit conn: Connection): Option[Contract] =
    SQL_SELECT_CONTRACT
      .on(
        "contract_id" -> contractId.coid,
        "party" -> forParty
      )
      .as(ContractDataParser.singleOpt)
      .map(mapContractDetails)

  private def lookupContractLetSync(contractId: AbsoluteContractId)(
      implicit conn: Connection): Option[LetLookup] =
    SQL_SELECT_CONTRACT_LET
      .on("contract_id" -> contractId.coid)
      .as(ContractLetParser.singleOpt)
      .map {
        case (_, None) => LetUnknown
        case (_, Some(let)) => Let(let.toInstant)
      }

  override def lookupActiveOrDivulgedContract(
      contractId: AbsoluteContractId,
      forParty: Party): Future[Option[Contract]] =
    dbDispatcher.executeSql("lookup_active_contract") { implicit conn =>
      lookupContractSync(contractId, forParty)
    }

  private def mapContractDetails(
      contractResult: (
          ContractIdString,
          Option[TransactionIdString],
          Option[EventId],
          Option[WorkflowId],
          Option[Date],
          InputStream,
          Option[InputStream],
          Option[String],
          Option[String]))(implicit conn: Connection): Contract =
    contractResult match {
      case (coid, None, None, None, None, contractStream, None, None, None) =>
        val divulgences = lookupDivulgences(coid)
        val absoluteCoid = AbsoluteContractId(coid)

        DivulgedContract(
          absoluteCoid,
          contractSerializer
            .deserializeContractInstance(ByteStreams.toByteArray(contractStream))
            .getOrElse(sys.error(s"failed to deserialize contract! cid:$coid")),
          divulgences
        )

      case (
          coid,
          Some(transactionId),
          Some(eventId),
          workflowId,
          Some(ledgerEffectiveTime),
          contractStream,
          keyStreamO,
          Some(signatoriesRaw),
          observersRaw) =>
        val witnesses = lookupWitnesses(coid)
        val divulgences = lookupDivulgences(coid)
        val absoluteCoid = AbsoluteContractId(coid)
        val contractInstance = contractSerializer
          .deserializeContractInstance(ByteStreams.toByteArray(contractStream))
          .getOrElse(sys.error(s"failed to deserialize contract! cid:$coid"))

        val signatories =
          signatoriesRaw.split(JdbcLedgerDao.PARTY_SEPARATOR).toSet.map(Party.assertFromString)
        val observers = observersRaw
          .map(_.split(JdbcLedgerDao.PARTY_SEPARATOR).toSet.map(Party.assertFromString))
          .getOrElse(Set.empty)

        ActiveContract(
          absoluteCoid,
          ledgerEffectiveTime.toInstant,
          transactionId,
          eventId,
          workflowId,
          contractInstance,
          witnesses,
          divulgences,
          keyStreamO.map(keyStream => {
            val keyMaintainers = lookupKeyMaintainers(coid)
            val keyValue = valueSerializer
              .deserializeValue(ByteStreams.toByteArray(keyStream))
              .getOrElse(sys.error(s"failed to deserialize key value! cid:$coid"))
            KeyWithMaintainers(keyValue, keyMaintainers)
          }),
          signatories,
          observers,
          contractInstance.agreementText
        )

      case (_, _, _, _, _, _, _, _, _) =>
        sys.error(
          "mapContractDetails called with partial data, can not map to either active or divulged contract")
    }

  private def lookupWitnesses(coid: String)(implicit conn: Connection): Set[Party] =
    SQL_SELECT_WITNESS
      .on("contract_id" -> coid)
      .as(party("witness").*)
      .toSet

  private def lookupDivulgences(coid: String)(
      implicit conn: Connection): Map[Party, TransactionIdString] =
    SQL_SELECT_DIVULGENCE
      .on("contract_id" -> coid)
      .as(DivulgenceParser.*)
      .map {
        case (party, _, transaction_id) => party -> transaction_id
      }
      .toMap

  private def lookupKeyMaintainers(coid: String)(implicit conn: Connection) =
    SQL_SELECT_KEY_MAINTAINERS
      .on("contract_id" -> coid)
      .as(party("maintainer").*)
      .toSet

  private val SQL_GET_LEDGER_ENTRIES = SQL(
    "select * from ledger_entries where ledger_offset>={startInclusive} and ledger_offset<{endExclusive} order by ledger_offset asc")

  // Note that here we are reading, non transactionally, the stream in chunks. The reason why this is
  // safe is that
  // * The ledger entries are never removed;
  // * We fix the ledger end at the beginning.
  private def paginatingStream[T](
      startInclusive: Long,
      endExclusive: Long,
      pageSize: Int,
      queryPage: (Long, Long) => Future[T]): Source[T, NotUsed] = {
    Source.unfoldAsync(startInclusive) { start =>
      if (start >= endExclusive) {
        Future.successful(None)
      } else {
        val pageEnd =
          if (endExclusive - startInclusive <= pageSize) endExclusive else start + pageSize
        queryPage(start, pageEnd).map { result =>
          Some(pageEnd -> result)
        }(executionContext)
      }
    }
  }

  private val PageSize = 100

  override def getLedgerEntries(
      startInclusive: Long,
      endExclusive: Long): Source[(Long, LedgerEntry), NotUsed] =
    paginatingStream(
      startInclusive,
      endExclusive,
      PageSize,
      (startI, endE) => {
        dbDispatcher.executeSql(s"load_ledger_entries", Some(s"bounds: [$startI, $endE[")) {
          implicit conn =>
            val parsedEntries = SQL_GET_LEDGER_ENTRIES
              .on("startInclusive" -> startI, "endExclusive" -> endE)
              .as(EntryParser.*)
            parsedEntries.map(entry => entry -> loadDisclosureOptForEntry(entry))
        }
      }
    ).mapConcat(identity)
      .map((toLedgerEntry _).tupled)

  private def loadDisclosureOptForEntry(parsedEntry: ParsedEntry)(
      implicit conn: Connection): Option[Relation[EventId, Party]] = {
    parsedEntry.transactionId.map { transactionId =>
      SQL_SELECT_DISCLOSURE
        .on("transaction_id" -> transactionId)
        .as(DisclosureParser.*)
        .groupBy(_._1)
        .transform((_, v) => v.map(_._2).toSet)
    }
  }

  // this query pre-filters the active contracts. this avoids loading data that anyway will be dismissed later
  private val SQL_SELECT_ACTIVE_CONTRACTS = SQL(queries.SQL_SELECT_ACTIVE_CONTRACTS)

  override def getActiveContractSnapshot(untilExclusive: LedgerOffset, filter: TemplateAwareFilter)(
      implicit mat: Materializer): Future[LedgerSnapshot] = {

    def orEmptyStringList(xs: Seq[String]) = if (xs.nonEmpty) xs else List("")

    def contractStream(conn: Connection, offset: Long): Source[ActiveContract, Future[Done]] = {
      //TODO: investigate where Akka Streams is actually iterating on the JDBC ResultSet (because, that is blocking IO!)
      AkkaStream
        .source(
          SQL_SELECT_ACTIVE_CONTRACTS.on(
            "offset" -> offset,
            // using '&' as a "separator" for the two columns because it is not allowed in either Party or Identifier strings
            // and querying on tuples is basically impossible to do sensibly.
            "template_parties" -> orEmptyStringList(filter.specificSubscriptions.map {
              case (ident, party) => ident.qualifiedName.qualifiedName + "&" + party.toString
            }),
            "wildcard_parties" -> orEmptyStringList(filter.globalSubscriptions.toList)
          ),
          parser = ContractDataParser
        )(mat, conn)
        .mapAsync(dbDispatcher.noOfShortLivedConnections) { contractResult =>
          // it's ok to not have query isolation as witnesses cannot change once we saved them
          dbDispatcher
            .executeSql("load_contract_details", Some(s"contract details: ${contractResult._1}")) {
              implicit conn =>
                mapContractDetails(contractResult) match {
                  case ac: ActiveContract => ac
                  case _: DivulgedContract =>
                    sys.error(
                      "Impossible: SQL_SELECT_ACTIVE_CONTRACTS returned a divulged contract")
                }
            }
        }
    }.mapMaterializedValue(_.map(_ => Done)(DirectExecutionContext))

    Future.successful(
      LedgerSnapshot(
        untilExclusive,
        dbDispatcher.runStreamingSql(conn => contractStream(conn, untilExclusive))))
  }

  private val SQL_SELECT_PARTIES =
    SQL("select party, display_name, ledger_offset, explicit from parties")

  case class ParsedPartyData(
      party: String,
      displayName: Option[String],
      ledgerOffset: Long,
      explicit: Boolean)

  private val PartyDataParser: RowParser[ParsedPartyData] =
    Macro.parser[ParsedPartyData](
      "party",
      "display_name",
      "ledger_offset",
      "explicit"
    )

  override def getParties: Future[List[PartyDetails]] =
    dbDispatcher.executeSql("load_parties") { implicit conn =>
      SQL_SELECT_PARTIES
        .as(PartyDataParser.*)
        // TODO: isLocal should be based on equality of participantId reported in an
        // update and the id given to participant in a command-line argument
        // (See issue #2026)
        .map(d => PartyDetails(Party.assertFromString(d.party), d.displayName, isLocal = true))
    }

  private val SQL_INSERT_PARTY =
    SQL("""insert into parties(party, display_name, ledger_offset, explicit)
        |select {party}, {display_name}, ledger_end, 'true'
        |from parameters""".stripMargin)

  override def storeParty(
      party: Party,
      displayName: Option[String],
      externalOffset: Option[ExternalOffset]): Future[PersistenceResponse] = {
    dbDispatcher.executeSql("store_party", Some(s"party: $party")) { implicit conn =>
      Try {
        SQL_INSERT_PARTY
          .on(
            "party" -> (party: String),
            "display_name" -> displayName,
          )
          .execute()
        externalOffset.foreach(updateExternalLedgerEnd)
        PersistenceResponse.Ok
      }.recover {
        case NonFatal(e) if e.getMessage.contains(queries.DUPLICATE_KEY_ERROR) =>
          logger.warn("Party with ID {} already exists", party)
          conn.rollback()
          PersistenceResponse.Duplicate
      }.get
    }
  }

  private val SQL_SELECT_PACKAGES =
    SQL("""select package_id, source_description, known_since, size
          |from packages
          |""".stripMargin)

  private val SQL_SELECT_PACKAGE =
    SQL("""select package
          |from packages
          |where package_id = {package_id}
          |""".stripMargin)

  case class ParsedPackageData(
      packageId: String,
      sourceDescription: Option[String],
      size: Long,
      knownSince: Date)

  private val PackageDataParser: RowParser[ParsedPackageData] =
    Macro.parser[ParsedPackageData](
      "package_id",
      "source_description",
      "size",
      "known_since"
    )

  override def listLfPackages: Future[Map[PackageId, PackageDetails]] =
    dbDispatcher.executeSql("load_packages") { implicit conn =>
      SQL_SELECT_PACKAGES
        .as(PackageDataParser.*)
        .map(
          d =>
            PackageId.assertFromString(d.packageId) -> PackageDetails(
              d.size,
              d.knownSince.toInstant,
              d.sourceDescription))
        .toMap
    }

  override def getLfArchive(packageId: PackageId): Future[Option[Archive]] =
    dbDispatcher.executeSql("load_archive", Some(s"pkg id: $packageId")) { implicit conn =>
      SQL_SELECT_PACKAGE
        .on(
          "package_id" -> packageId
        )
        .as[Option[Array[Byte]]](SqlParser.byteArray("package").singleOpt)
        .map(data => Archive.parseFrom(Decode.damlLfCodedInputStreamFromBytes(data)))
    }

  override def uploadLfPackages(
      uploadId: String,
      packages: List[(Archive, PackageDetails)],
      externalOffset: Option[ExternalOffset]): Future[Map[PersistenceResponse, Int]] = {
    val requirements = Try {
      require(uploadId.nonEmpty, "The upload identifier cannot be the empty string")
      require(packages.nonEmpty, "The list of packages to upload cannot be empty")
    }
    requirements.fold(
      Future.failed,
      _ =>
        dbDispatcher.executeSql(
          "store_packages",
          Some(s"packages: ${packages.map(_._1.getHash).mkString(", ")}")) { implicit conn =>
          externalOffset.foreach(updateExternalLedgerEnd)
          val params = packages
            .map(
              p =>
                Seq[NamedParameter](
                  "package_id" -> p._1.getHash,
                  "upload_id" -> uploadId,
                  "source_description" -> p._2.sourceDescription,
                  "size" -> p._2.size,
                  "known_since" -> p._2.knownSince,
                  "package" -> p._1.toByteArray
              )
            )
          val updated =
            executeBatchSql(queries.SQL_INSERT_PACKAGE, params).map(math.max(0, _)).sum
          val duplicates = packages.length - updated

          Map(
            PersistenceResponse.Ok -> updated,
            PersistenceResponse.Duplicate -> duplicates
          ).filter(_._2 > 0)
      }
    )
  }

  private val SQL_TRUNCATE_ALL_TABLES =
    SQL("""
        |truncate ledger_entries cascade;
        |truncate disclosures cascade;
        |truncate contracts cascade;
        |truncate contract_data cascade;
        |truncate contract_witnesses cascade;
        |truncate contract_key_maintainers cascade;
        |truncate parameters cascade;
        |truncate contract_keys cascade;
      """.stripMargin)

  override def reset(): Future[Unit] =
    dbDispatcher.executeSql("truncate_all_tables") { implicit conn =>
      val _ = SQL_TRUNCATE_ALL_TABLES.execute()
      ()
    }

  override def close(): Unit =
    dbDispatcher.close()

  private def executeBatchSql(query: String, params: Iterable[Seq[NamedParameter]])(
      implicit con: Connection) = {
    require(params.nonEmpty, "batch sql statement must have at least one set of name parameters")
    BatchSql(query, params.head, params.drop(1).toArray: _*).execute()
  }

}

object JdbcLedgerDao {
  def apply(
      dbDispatcher: DbDispatcher,
      contractSerializer: ContractSerializer,
      transactionSerializer: TransactionSerializer,
      valueSerializer: ValueSerializer,
      keyHasher: KeyHasher,
      dbType: DbType,
      loggerFactory: NamedLoggerFactory,
      executionContext: ExecutionContext): LedgerDao =
    new JdbcLedgerDao(
      dbDispatcher,
      contractSerializer,
      transactionSerializer,
      valueSerializer,
      keyHasher,
      dbType,
      loggerFactory,
      executionContext)

  private val PARTY_SEPARATOR = '%'

  sealed trait Queries {

    // SQL statements using the proprietary Postgres on conflict .. do nothing clause
    protected[JdbcLedgerDao] def SQL_INSERT_CONTRACT_DATA: String
    protected[JdbcLedgerDao] def SQL_INSERT_PACKAGE: String
    protected[JdbcLedgerDao] def SQL_IMPLICITLY_INSERT_PARTIES: String

    protected[JdbcLedgerDao] def SQL_SELECT_CONTRACT: String
    protected[JdbcLedgerDao] def SQL_SELECT_ACTIVE_CONTRACTS: String

    // Note: the SQL backend may receive divulgence information for the same (contract, party) tuple
    // more than once through BlindingInfo.globalDivulgence.
    // The ledger offsets for the same (contract, party) tuple should always be increasing, and the database
    // stores the offset at which the contract was first disclosed.
    // We therefore don't need to update anything if there is already some data for the given (contract, party) tuple.
    protected[JdbcLedgerDao] def SQL_BATCH_INSERT_DIVULGENCES: String
    protected[JdbcLedgerDao] def SQL_BATCH_INSERT_DIVULGENCES_FROM_TRANSACTION_ID: String

    protected[JdbcLedgerDao] def DUPLICATE_KEY_ERROR
      : String // TODO: Avoid brittleness of error message checks
  }

  object PostgresQueries extends Queries {

    override protected[JdbcLedgerDao] val SQL_INSERT_CONTRACT_DATA: String =
      """insert into contract_data(id, contract) values({id}, {contract})
        |on conflict (id) do nothing""".stripMargin

    override protected[JdbcLedgerDao] val SQL_INSERT_PACKAGE: String =
      """insert into packages(package_id, upload_id, source_description, size, known_since, ledger_offset, package)
        |select {package_id}, {upload_id}, {source_description}, {size}, {known_since}, ledger_end, {package}
        |from parameters
        |on conflict (package_id) do nothing""".stripMargin

    override protected[JdbcLedgerDao] val SQL_IMPLICITLY_INSERT_PARTIES: String =
      """insert into parties(party, explicit, ledger_offset)
        |values({name}, {explicit}, {ledger_offset})
        |on conflict (party) do nothing""".stripMargin

    override protected[JdbcLedgerDao] val SQL_BATCH_INSERT_DIVULGENCES: String =
      """insert into contract_divulgences(contract_id, party, ledger_offset, transaction_id)
        |values({contract_id}, {party}, {ledger_offset}, {transaction_id})
        |on conflict on constraint contract_divulgences_idx do nothing""".stripMargin

    override protected[JdbcLedgerDao] val SQL_BATCH_INSERT_DIVULGENCES_FROM_TRANSACTION_ID: String =
      """insert into contract_divulgences(contract_id, party, ledger_offset, transaction_id)
        |select {contract_id}, {party}, ledger_offset, {transaction_id}
        |from ledger_entries
        |where transaction_id={transaction_id}
        |on conflict on constraint contract_divulgences_idx do nothing""".stripMargin

    override protected[JdbcLedgerDao] val DUPLICATE_KEY_ERROR: String = "duplicate key"

    override protected[JdbcLedgerDao] val SQL_SELECT_CONTRACT: String =
      s"""
         |select
         |  cd.id,
         |  cd.contract,
         |  c.transaction_id,
         |  c.create_event_id,
         |  c.workflow_id,
         |  c.key,
         |  le.effective_at,
         |  string_agg(distinct sigs.signatory, '$PARTY_SEPARATOR') as signatories,
         |  string_agg(distinct obs.observer, '$PARTY_SEPARATOR') as observers
         |from contract_data cd
         |left join contracts c on cd.id=c.id
         |left join ledger_entries le on c.transaction_id = le.transaction_id
         |left join contract_witnesses cowi on cowi.contract_id = c.id and witness = {party}
         |left join contract_divulgences codi on codi.contract_id = cd.id and party = {party}
         |left join contract_signatories sigs on sigs.contract_id = c.id
         |left join contract_observers obs on obs.contract_id = c.id
         |
         |where
         |  cd.id={contract_id} and
         |  c.archive_offset is null and
         |  (cowi.witness is not null or codi.party is not null)
         |group by cd.id, cd.contract, c.transaction_id, c.create_event_id, c.workflow_id, c.key, le.effective_at
         |""".stripMargin

    override protected[JdbcLedgerDao] def SQL_SELECT_ACTIVE_CONTRACTS: String =
      // the distinct keyword is required, because a single contract can be visible by 2 parties,
      // thus resulting in multiple output rows
      s"""
         |select distinct
         |  cd.id,
         |  cd.contract,
         |  c.transaction_id,
         |  c.create_event_id,
         |  c.workflow_id,
         |  c.key,
         |  le.effective_at,
         |  string_agg(distinct sigs.signatory, '$PARTY_SEPARATOR') as signatories,
         |  string_agg(distinct obs.observer, '$PARTY_SEPARATOR') as observers
         |from contracts c
         |inner join contract_data cd on c.id = cd.id
         |inner join ledger_entries le on c.transaction_id = le.transaction_id
         |inner join contract_witnesses w on c.id = w.contract_id
         |left join contract_signatories sigs on sigs.contract_id = c.id
         |left join contract_observers obs on obs.contract_id = c.id
         |where create_offset <= {offset} and (archive_offset is null or archive_offset > {offset})
         |and
         |   (
         |     concat(c.name,'&',w.witness) in ({template_parties})
         |     OR w.witness in ({wildcard_parties})
         |    )
         |group by cd.id, cd.contract, c.transaction_id, c.create_event_id, c.workflow_id, c.key, le.effective_at
         |""".stripMargin
  }

  object H2DatabaseQueries extends Queries {

    override protected[JdbcLedgerDao] val SQL_INSERT_CONTRACT_DATA: String =
      """merge into contract_data using dual on id = {id}
        |when not matched then insert (id, contract) values ({id}, {contract})""".stripMargin

    override protected[JdbcLedgerDao] val SQL_INSERT_PACKAGE: String =
      """merge into packages using dual on package_id = {package_id}
        |when not matched then insert (package_id, upload_id, source_description, size, known_since, ledger_offset, package)
        |select {package_id}, {upload_id}, {source_description}, {size}, {known_since}, ledger_end, {package}
        |from parameters""".stripMargin

    override protected[JdbcLedgerDao] val SQL_IMPLICITLY_INSERT_PARTIES: String =
      """merge into parties using dual on party = {name}
        |when not matched then insert (party, explicit, ledger_offset) values ({name}, {explicit}, {ledger_offset})""".stripMargin

    override protected[JdbcLedgerDao] val SQL_BATCH_INSERT_DIVULGENCES: String =
      """merge into contract_divulgences using dual on contract_id = {contract_id} and party = {party}
        |when not matched then insert (contract_id, party, ledger_offset, transaction_id)
        |values ({contract_id}, {party}, {ledger_offset}, {transaction_id})""".stripMargin

    override protected[JdbcLedgerDao] val SQL_BATCH_INSERT_DIVULGENCES_FROM_TRANSACTION_ID: String =
      """merge into contract_divulgences using dual on contract_id = {contract_id} and party = {party}
        |when not matched then insert (contract_id, party, ledger_offset, transaction_id)
        |select {contract_id}, {party}, ledger_offset, {transaction_id}
        |from ledger_entries
        |where transaction_id={transaction_id}""".stripMargin

    override protected[JdbcLedgerDao] val DUPLICATE_KEY_ERROR: String =
      "Unique index or primary key violation"

    override protected[JdbcLedgerDao] val SQL_SELECT_CONTRACT: String =
      s"""
         |select
         |  cd.id,
         |  cd.contract,
         |  c.transaction_id,
         |  c.create_event_id,
         |  c.workflow_id,
         |  c.key,
         |  le.effective_at,
         |  listagg(distinct sigs.signatory, '$PARTY_SEPARATOR') as signatories,
         |  listagg(distinct obs.observer, '$PARTY_SEPARATOR') as observers
         |from contract_data cd
         |left join contracts c on cd.id=c.id
         |left join ledger_entries le on c.transaction_id = le.transaction_id
         |left join contract_witnesses cowi on cowi.contract_id = c.id and witness = {party}
         |left join contract_divulgences codi on codi.contract_id = cd.id and party = {party}
         |left join contract_signatories sigs on sigs.contract_id = c.id
         |left join contract_observers obs on obs.contract_id = c.id
         |
         |where
         |  cd.id={contract_id} and
         |  c.archive_offset is null and
         |  (cowi.witness is not null or codi.party is not null)
         |group by cd.id, cd.contract, c.transaction_id, c.create_event_id, c.workflow_id, c.key, le.effective_at
         |""".stripMargin

    override protected[JdbcLedgerDao] def SQL_SELECT_ACTIVE_CONTRACTS: String =
      // the distinct keyword is required, because a single contract can be visible by 2 parties,
      // thus resulting in multiple output rows
      s"""
         |select distinct
         |  cd.id,
         |  cd.contract,
         |  c.transaction_id,
         |  c.create_event_id,
         |  c.workflow_id,
         |  c.key,
         |  le.effective_at,
         |  listagg(distinct sigs.signatory, '$PARTY_SEPARATOR') as signatories,
         |  listagg(distinct obs.observer, '$PARTY_SEPARATOR') as observers
         |from contracts c
         |inner join contract_data cd on c.id = cd.id
         |inner join ledger_entries le on c.transaction_id = le.transaction_id
         |inner join contract_witnesses w on c.id = w.contract_id
         |left join contract_signatories sigs on sigs.contract_id = c.id
         |left join contract_observers obs on obs.contract_id = c.id
         |where create_offset <= {offset} and (archive_offset is null or archive_offset > {offset})
         |and
         |   (
         |     concat(c.name,'&',w.witness) in ({template_parties})
         |     OR w.witness in ({wildcard_parties})
         |    )
         |group by cd.id, cd.contract, c.transaction_id, c.create_event_id, c.workflow_id, c.key, le.effective_at
         |""".stripMargin

  }
}
