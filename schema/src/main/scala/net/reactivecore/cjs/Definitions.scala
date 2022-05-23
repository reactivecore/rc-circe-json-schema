package net.reactivecore.cjs

import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import io.circe.syntax._
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}
import net.reactivecore.cjs.validator.Validator.Compound

/** Describes sub definitions. */
case class Definitions(
    defs: Option[VectorMap[String, Schema]] = None
)

object Definitions {
  val JsonKey = "$defs"

  implicit val decoder: Decoder[Definitions] = Decoder { json =>
    for {
      defs <- json.get[Option[VectorMap[String, Schema]]](JsonKey)
    } yield Definitions(defs)
  }

  implicit val encoder: Encoder.AsObject[Definitions] = Codecs.withoutNulls {
    Encoder.AsObject.instance[Definitions] { value =>
      JsonObject(
        JsonKey -> value.defs.asJson
      )
    }
  }

  implicit lazy val codec: Codec.AsObject[Definitions] = Codec.AsObject.from(decoder, encoder)

  /** Validator for Definitions. Note: the validator itself doesn't really validate, but contains validators */
  case class DefinitionsValidator(underlying: Vector[Validator]) extends Compound(underlying) {

    override def touch(state: ValidationState): ValidationState = {
      underlying.foldLeft(state) { case (s, u) => u.touch(s) }
    }

    override def validateStateful(state: ValidationState, json: Json)(
        implicit context: ValidationContext
    ): (ValidationState, ValidationResult) = {
      touch(state) -> ValidationResult.success
    }

    override def precedence: Int = {
      // Definitions like to set anchors, so we want them before
      -1
    }
  }

  implicit lazy val validationProvider: ValidationProvider[Definitions] = ValidationProvider.withOrigin {
    (context, definitions) =>
      val subPath = context.enterObject(JsonKey)
      val allDefinitions = definitions.defs.getOrElse(VectorMap.empty)
      if (allDefinitions.isEmpty) {
        Validator.success
      } else {

        val myChildren = allDefinitions.map { case (key, schema) =>
          val childPath = subPath.enterObject(key)
          val subValidator = schema.validator(childPath)
          subValidator
        }.toVector
        DefinitionsValidator(myChildren)
      }
  }
}
