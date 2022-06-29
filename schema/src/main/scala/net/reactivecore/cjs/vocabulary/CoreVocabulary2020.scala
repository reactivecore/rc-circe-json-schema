package net.reactivecore.cjs.vocabulary

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.{DefinitionsValidator, ObjectSchemaValidator, ValidationProvider, Validator}

object CoreVocabulary2020 extends Vocabulary {
  override val name: String = Names.V2020_12.core

  case class Implementation(
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

  override def codec(implicit schemaCodec: Codec[Schema]): Codec[Implementation] =
    Codecs.withoutNulls(semiauto.deriveCodec)

  override def validationProvider: ValidationProvider[Implementation] = ValidationProvider.withOrigin {
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
