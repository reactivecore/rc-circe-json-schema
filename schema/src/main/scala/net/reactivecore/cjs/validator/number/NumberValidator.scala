package net.reactivecore.cjs.validator.number

import io.circe.Json
import net.reactivecore.cjs.validator.SimpleContextFreeValidator

abstract class NumberValidator(name: String, isValidNumber: BigDecimal => Boolean)
    extends SimpleContextFreeValidator(name) {
  override def isValid(json: Json): Boolean = {
    json.as[BigDecimal] match {
      case Left(_)  => true
      case Right(c) => isValidNumber(c)
    }
  }
}

case class MinimumValidator(value: BigDecimal) extends NumberValidator("minimum", _ >= value)

case class ExclusiveMinimumValidator(value: BigDecimal) extends NumberValidator("exclusiveMinimum", _ > value)

case class MaximumValidator(value: BigDecimal) extends NumberValidator("maximum", _ <= value)

case class ExclusiveMaximumValidator(value: BigDecimal) extends NumberValidator("exclusiveMaximum", _ < value)

case class MultipleOfValidator(value: BigDecimal) extends NumberValidator("multipleOf", _ % value == 0)
