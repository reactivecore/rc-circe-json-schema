package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.restriction.ArrayRestriction.{AdditionalItems, Items}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.array._
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

case class ArrayRestriction(
    items: OValidatingField[Either[Schema, Vector[Schema]], Items.type] = None, // Left: V2020_12, Right: V2019_09
    contains: OValidatingField[Schema, ContainsValidator] = None,
    minItems: OValidatingField[Long, SimpleValidator.MinItems] = None,
    maxItems: OValidatingField[Long, SimpleValidator.MaxItems] = None,
    uniqueItems: OValidatingField[Boolean, SimpleValidator.Unique.type] = None,
    unevaluatedItems: OValidatingField[Schema, UnevaluatedItemsValidator] = None,
    prefixItems: OValidatingField[Vector[Schema], PrefixValidator] = None,
    minContains: OValidatingField[Int, Validator.Success.type] = None,
    maxContains: OValidatingField[Int, Validator.Success.type] = None,
    additionalItems: OValidatingField[Schema, AdditionalItems.type] = None // V2019_09
) {

  /** Items in V2019 Notation */
  def v2019Items: Option[Vector[Schema]] = {
    items.flatMap(_.value.right.toOption)
  }

  /** Items in V2020 Notation */
  def v2020Items: Option[Schema] = {
    items.flatMap(_.value.left.toOption)
  }

  /** Returns the number of items, the restriction defines using prefixItems or V2019 Items notation. */
  def effectivePrefixSize: Int = {
    prefixItems.map(_.value).orElse(v2019Items).map(_.size).getOrElse(0)
  }
}

object ArrayRestriction {
  private implicit def eitherSchemaOrArrayCodec: Codec[Either[Schema, Vector[Schema]]] = {
    Codecs.disjunctEitherCodec[Schema, Vector[Schema]]
  }

  implicit lazy val codec: Codec.AsObject[ArrayRestriction] =
    Codecs.withoutNulls(semiauto.deriveCodec[ArrayRestriction])

  // Support for Items (can be 2019 or 2020)
  case object Items
  implicit val itemsValidatorProvider: ValidationProvider[
    (ValidatingField[Either[Schema, Vector[Schema]], Items.type], ArrayRestriction)
  ] =
    ValidationProvider.forFieldWithContext { (origin, value, context) =>
      value match {
        case Left(value)  => ItemValidator(origin, value, context.effectivePrefixSize)
        case Right(value) => PrefixValidator(origin, value)
      }
    }

  // Support for AdditionalItems
  object AdditionalItems
  implicit val additionalItemsProvider
      : ValidationProvider[(ValidatingField[Schema, AdditionalItems.type], ArrayRestriction)] =
    ValidationProvider.forFieldWithContext[Schema, AdditionalItems.type, ArrayRestriction] { (origin, value, context) =>
      // V2019, additionalItems is only used if items is given
      if (context.v2019Items.isDefined) {
        ItemValidator(origin, value, context.effectivePrefixSize)
      } else {
        Validator.success
      }

    }

  implicit val validationProvider: ValidationProvider[ArrayRestriction] =
    ValidationProvider.visitingSequental[ArrayRestriction]
}
