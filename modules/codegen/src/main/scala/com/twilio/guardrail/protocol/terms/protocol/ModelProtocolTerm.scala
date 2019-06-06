package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }

sealed trait ModelProtocolTerm[L <: LA, T]
case class ExtractProperties[L <: LA](swagger: Schema[_]) extends ModelProtocolTerm[L, List[(String, Schema[_])]]
case class RenderDTOClass[L <: LA](clsName: String, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, L#ClassDefinition]
case class EncodeModel[L <: LA](clsName: String, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, Option[L#ValueDefinition]]
case class DecodeModel[L <: LA](clsName: String, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, Option[L#ValueDefinition]]
case class RenderDTOStaticDefns[L <: LA](clsName: String, deps: List[L#TermName], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition])
    extends ModelProtocolTerm[L, StaticDefns[L]]
