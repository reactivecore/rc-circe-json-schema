package net.reactivecore.cjs

import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import io.circe.syntax._
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}

/** Contains reference fields within a Schema. */
case class Ref(
    ref: Option[RefUri] = None,
    anchor: Option[String] = None,
    dynamicRef: Option[RefUri] = None,
    dynamicAnchor: Option[String] = None,
    recursiveAnchor: Option[Boolean] = None, // 2019_09
    recursiveRef: Option[RefUri] = None // 2019_09
) {

  /** Use either dynamicRef or resursive Ref (compatibility reasons) */
  def effectiveDynamicRef: Option[RefUri] = dynamicRef.orElse(recursiveRef)
}

object Ref {
  implicit val refEncoder: Encoder.AsObject[Ref] = Encoder.AsObject.instance[Ref] { value =>
    JsonObject(
      "$ref" -> value.ref.asJson,
      "$anchor" -> value.anchor.asJson,
      "$dynamicRef" -> value.dynamicRef.asJson,
      "$dynamicAnchor" -> value.dynamicAnchor.asJson,
      "$recursiveAnchor" -> value.recursiveAnchor.asJson,
      "$recursiveRef" -> value.recursiveRef.asJson
    )
  }

  implicit val refDecoder: Decoder[Ref] = Decoder.instance { json =>
    for {
      ref <- json.get[Option[RefUri]]("$ref")
      anchor <- json.get[Option[String]]("$anchor")
      dynamicRef <- json.get[Option[RefUri]]("$dynamicRef")
      dynamicAnchor <- json.get[Option[String]]("$dynamicAnchor")
      recursiveAnchor <- json.get[Option[Boolean]]("$recursiveAnchor")
      recursiveRef <- json.get[Option[RefUri]]("$recursiveRef")
    } yield Ref(ref, anchor, dynamicRef, dynamicAnchor, recursiveAnchor, recursiveRef)
  }

  implicit lazy val refCodec: Codec.AsObject[Ref] = Codecs.withoutNulls(Codec.AsObject.from(refDecoder, refEncoder))

  case class RefValidator(ref: RefUri, fullPath: RefUri) extends Validator {
    override def validateStateful(state: ValidationState, json: Json)(
        implicit context: ValidationContext
    ): (ValidationState, ValidationResult) = {
      context.resolve(fullPath) match {
        case Left(error) =>
          state -> ValidationResult.violation(json, s"Invalid ref: ${fullPath} (from ${ref}): ${error}")
        case Right(ok) => ok.validateStateful(state, json)
      }
    }
  }

  case class DynamicRefValidator(ref: RefUri, fullPath: RefUri) extends Validator {
    override def validateStateful(state: ValidationState, json: Json)(
        implicit context: ValidationContext
    ): (ValidationState, ValidationResult) = {
      context.resolveDynamic(state, fullPath) match {
        case Left(error) =>
          state -> ValidationResult.violation(json, s"Invalid dynamicRef: ${fullPath} (from ${ref}): ${error}")
        case Right(ok) => ok.validateStateful(state, json)
      }
    }
  }

  implicit val validationProvider: ValidationProvider[Ref] = ValidationProvider.withOrigin { (origin, instance) =>
    Validator.sequenceOfOpts(
      instance.ref.map { ref =>
        val fullPath = origin.parentId.resolve(ref)
        RefValidator(ref, fullPath)
      },
      instance.effectiveDynamicRef.map { dynamicRef =>
        val fullPath = origin.parentId.resolve(dynamicRef)
        DynamicRefValidator(dynamicRef, fullPath)
      }
    )
  }
}
