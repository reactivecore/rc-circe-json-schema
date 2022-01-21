package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

import java.util.regex.Pattern

case class StringRestriction(
    format: Option[String] = None,
    pattern: Option[String] = None,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    contentMediaType: Option[String] = None,
    contentEncoding: Option[String] = None,
    contentSchema: Option[Schema] = None
)

object StringRestriction {
  implicit lazy val codec: Codec.AsObject[StringRestriction] =
    Codecs.withoutNulls(semiauto.deriveCodec[StringRestriction])

  implicit val validationProvider: ValidationProvider[StringRestriction] = ValidationProvider.instance { s =>
    Validator.sequenceOfOpts(
      s.pattern.map { pattern =>
        // Note: this is not secure on Java < 9
        val compiled = Pattern.compile(pattern)
        stringValidator("pattern") { s =>
          compiled.matcher(s).find()
        }
      },
      s.minLength.map { minLength =>
        stringValidator("minLength") { s => codePointLength(s) >= minLength }
      },
      s.maxLength.map { maxLength =>
        stringValidator("maxLength") { s => codePointLength(s) <= maxLength }
      }
    )
  }

  /** We need the number of codepoints for string length comparisons. */
  private def codePointLength(s: String): Int = {
    s.codePointCount(0, s.length)
  }

  private def stringValidator(name: String)(f: String => Boolean): Validator = Validator.simple(name) { json =>
    json.asString.forall(f)
  }
}
