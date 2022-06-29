package net.reactivecore.cjs

import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import io.circe.syntax._
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.{
  DefinitionsValidator,
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
