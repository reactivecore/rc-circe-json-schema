package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.string.StringValidator.{MaxLengthValidator, MinLengthValidator, PatternValidator}
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

import java.util.regex.Pattern

case class StringRestriction(
    format: OValidatingField[String, Validator.Success.type] = None,
    pattern: OValidatingField[String, PatternValidator] = None,
    minLength: OValidatingField[Int, MinLengthValidator] = None,
    maxLength: OValidatingField[Int, MaxLengthValidator] = None,
    contentMediaType: OValidatingField[String, Validator.Success.type] = None,
    contentEncoding: OValidatingField[String, Validator.Success.type] = None,
    contentSchema: OValidatingField[Schema, Validator.Success.type] = None
)

object StringRestriction {
  implicit lazy val codec: Codec.AsObject[StringRestriction] =
    Codecs.withoutNulls(semiauto.deriveCodec[StringRestriction])

  implicit val validationProvider: ValidationProvider[StringRestriction] = ValidationProvider.visitingSequental
}
