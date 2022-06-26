package net.reactivecore.cjs

import io.circe.Codec
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator.ValidationProvider

/** Constant values for vocabularies. */
object Vocabulary {
  object V2019_12 {
    val core = "https://json-schema.org/draft/2019-09/vocab/core"
    val applicator = "https://json-schema.org/draft/2019-09/vocab/applicator"
    val validation = "https://json-schema.org/draft/2019-09/vocab/validation"
    val metadata = "https://json-schema.org/draft/2019-09/vocab/meta-data"
    val format = "https://json-schema.org/draft/2019-09/vocab/format"
    val content = "https://json-schema.org/draft/2019-09/vocab/content"

    val all = Seq(core, applicator, validation, metadata, format, content)
  }

  object V2020_12 {
    val core = "https://json-schema.org/draft/2020-12/vocab/core"
    val applicator = "https://json-schema.org/draft/2020-12/vocab/applicator"
    val unevaluated = "https://json-schema.org/draft/2020-12/vocab/unevaluated"
    val validation = "https://json-schema.org/draft/2020-12/vocab/validation"
    val metadata = "https://json-schema.org/draft/2020-12/vocab/meta-data"
    val formatAnnotation = "https://json-schema.org/draft/2020-12/vocab/format-annotation"
    val content = "https://json-schema.org/draft/2020-12/vocab/content"

    val all = Seq(core, applicator, unevaluated, validation, metadata, formatAnnotation, content)
  }

  /** Full vocabulary. */
  val all = V2019_12.all ++ V2020_12.all
}

trait Vocabulary {
  val name: String

  type Implementation

  def codec(implicit schemaCodec: Codec[Schema]): Codec[Implementation]

  def validationProvider: ValidationProvider[Implementation]
}

object CoreVocabulary2020 extends Vocabulary {
  override val name: String = Vocabulary.V2020_12.core

  case class Implementation(
      `$id`: Option[RefUri] = None,
      `$schema`: Option[RefUri] = None,
      `$ref`: Option[RefUri] = None,
      `$anchor`: Option[String] = None,
      `dynamicRef`: Option[RefUri] = None,
      `$vocabulary`: Option[Map[String, Boolean]] = None,
      `$comment`: Option[String] = None,
      `$defs`: Option[VectorMap[String, Schema]] = None
  )

  override def codec: Codec[Implementation] = ???

  override def validationProvider: ValidationProvider[Implementation] = ???
}
