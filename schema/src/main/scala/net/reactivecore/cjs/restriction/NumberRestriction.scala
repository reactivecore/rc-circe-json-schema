package net.reactivecore.cjs.restriction

import io.circe.generic.semiauto
import io.circe.{Codec, Json}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{ValidationProvider, ValidationResult, Validator, Violation}

case class NumberRestriction(
    minimum: Option[BigDecimal] = None,
    exclusiveMinimum: Option[BigDecimal] = None,
    maximum: Option[BigDecimal] = None,
    exclusiveMaximum: Option[BigDecimal] = None,
    multipleOf: Option[BigDecimal] = None
) {
  def isEmpty: Boolean = this == NumberRestriction()
}

object NumberRestriction {
  implicit lazy val codec: Codec.AsObject[NumberRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit lazy val validationProvider: ValidationProvider[NumberRestriction] = ValidationProvider.instance {
    numberRestriction =>
      val numberChecks = buildNumberChecks(numberRestriction)

      if (numberRestriction.isEmpty) {
        Validator.success
      } else
        Validator.contextFree { json =>
          {
            ValidationResult(
              json
                .as[BigDecimal]
                .toSeq
                .flatMap { n =>
                  numberChecks(n).violations
                }
            )
          }
        }
  }

  private def buildNumberChecks(
      numberRestriction: NumberRestriction
  ): BigDecimal => ValidationResult = {
    def build(
        restriction: NumberRestriction => Option[BigDecimal],
        name: String,
        comparer: (BigDecimal, BigDecimal) => Boolean
    ): Option[BigDecimal => ValidationResult] = {
      restriction(numberRestriction).map { restriction => input =>
        if (comparer(restriction, input)) {
          ValidationResult.success
        } else {
          ValidationResult.violation(Json.fromBigDecimal(input), name)
        }
      }
    }

    val checks = Seq(
      build(_.minimum, "minimum", _ <= _),
      build(_.maximum, "maximum", _ >= _),
      build(_.exclusiveMinimum, "exclusiveMinimum", _ < _),
      build(_.exclusiveMaximum, "exclusiveMaximum", _ > _),
      build(_.multipleOf, "multipleOf", (r, n) => n % r == 0)
    ).flatten

    input => ValidationResult(checks.flatMap(_.apply(input).violations))
  }
}
