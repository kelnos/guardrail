package com.twilio.guardrail.generators
import com.twilio.guardrail.languages.LA

class GeneratorSettings[+L <: LA](val fileType: L#Type, val jsonType: L#Type)
