// Copyright (c) 2019 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.extractor.json

import com.digitalasset.daml.lf.data.SortedLookupList
import com.digitalasset.daml.lf.value.{Value => V}
import com.digitalasset.daml.lf.value.json.ApiCodecCompressed
import com.digitalasset.extractor.ledger.types.{Identifier, LedgerValue}
import com.digitalasset.extractor.ledger.types.LedgerValue._
import com.digitalasset.extractor.writers.postgresql.DataFormatState.MultiTableState
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._

object JsonConverters {
  private[this] object LfValueSprayEnc
      extends ApiCodecCompressed[String](
        encodeDecimalAsString = true,
        encodeInt64AsString = false
      ) {
    import spray.json._, ApiCodecCompressed.JsonImplicits.StringJsonFormat
    override protected[this] def apiContractIdToJsValue(v: String): JsValue = JsString(v)
    override protected[this] def jsValueToApiContractId(value: JsValue): String =
      value.convertTo[String]
  }

  private[this] def sprayToCirce(s: spray.json.JsValue): Json = {
    import spray.{json => sj}
    s match {
      case sj.JsString(v) => Json fromString v
      case sj.JsNumber(v) => Json fromBigDecimal v
      case sj.JsBoolean(v) => Json fromBoolean v
      case sj.JsObject(v) => Json fromFields (v transform ((_, e) => sprayToCirce(e)))
      case sj.JsArray(v) => Json fromValues (v map sprayToCirce)
      case sj.JsNull => Json.Null
    }
  }

  def toJsonString[A: Encoder](a: A): String = {
    a.asJson.noSpaces
  }

  implicit val recordEncoder: Encoder[OfCid[V.ValueRecord]] = valueEncoder

  implicit def valueEncoder[T <: LedgerValue]: Encoder[T] =
    t => sprayToCirce(LfValueSprayEnc.apiValueToJsValue(t))

  implicit val variantEncoder: Encoder[OfCid[V.ValueVariant]] = valueEncoder

  implicit val mapEncoder: Encoder[SortedLookupList[LedgerValue]] =
    valueEncoder.contramap(V.ValueTextMap(_))

  implicit val idKeyEncoder: KeyEncoder[Identifier] = id => s"${id.packageId}@${id.name}"
  implicit val idKeyDecoder: KeyDecoder[Identifier] = StringEncodedIdentifier.unapply

  private object StringEncodedIdentifier {
    private val idPattern = raw"(\w*)@(.*)".r

    def unapply(str: String): Option[Identifier] = str match {
      case idPattern(hash, name) => Some(Identifier(hash, name))
      case _ => None
    }
  }

  implicit val multiTableStateEncoder: Encoder[MultiTableState] = deriveEncoder[MultiTableState]
  implicit val multiTableStateDecoder: Decoder[MultiTableState] = deriveDecoder[MultiTableState]
}
