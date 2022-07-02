package net.reactivecore.cjs.vocabulary

import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.{DefinitionsValidator, ObjectSchemaValidator, ValidationProvider, Validator}

case class CoreVocabulary2020(
    `$id`: Option[RefUri] = None,
    `$schema`: Option[RefUri] = None,
    `$ref`: Option[RefUri] = None,
    `$anchor`: Option[String] = None,
    `dynamicRef`: Option[RefUri] = None,
    `$dynamicAnchor`: Option[String] = None,
    `$vocabulary`: Option[Map[String, Boolean]] = None,
    `$comment`: Option[String] = None,
    `$defs`: Option[VectorMap[String, Schema]] = None
)

object CoreVocabulary2020 extends TypedVocabularyPart[CoreVocabulary2020] {
  override val name: String = Names.V2020_12.core

  override def codec(implicit schemaCodec: Codec[Schema]): Codec.AsObject[CoreVocabulary2020] = {
    Codecs.withoutNulls(semiauto.deriveCodec)
  }

  override def validationProvider: ValidationProvider[CoreVocabulary2020] = ValidationProvider.withOrigin {
    (origin, value) =>
      // TODO: Special enter ID support?
      // TODO: Entered must also reach the other objects of the vocabulary
      val entered = value.`$id`.map { id =>
        origin.copy(
          parentId = id
        )
      }.getOrElse(origin)

      val definitionValidator = value.`$defs`.map { definitions =>
        val validators = definitions.map { case (key, schema) =>
          schema.validator(origin.enterObject(key))
        }.toVector
        DefinitionsValidator(validators)
      }.getOrElse(Validator.success)

      ObjectSchemaValidator(
        entered,
        definitionValidator,
        value.`$anchor`,
        dynamicFragment = value.`$dynamicAnchor`,
        idOverride = value.`$id`
      )
  }
}
