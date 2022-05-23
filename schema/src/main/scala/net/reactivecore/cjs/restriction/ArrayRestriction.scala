package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.restriction.ArrayRestriction.TypeOrTypesValidator
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.array._
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

case class ArrayRestriction(
    items: OValidatingField[Either[Schema, Vector[Schema]], TypeOrTypesValidator.type] =
      None, // Left: V2020_12, Right: V2019_09
    contains: OValidatingField[Schema, ContainsValidator] = None,
    minItems: OValidatingField[Long, SimpleValidator.MinItems] = None,
    maxItems: OValidatingField[Long, SimpleValidator.MaxItems] = None,
    uniqueItems: OValidatingField[Boolean, SimpleValidator.Unique.type] = None,
    unevaluatedItems: OValidatingField[Schema, UnevaluatedItemsValidator] = None,
    prefixItems: OValidatingField[Vector[Schema], PrefixValidator] = None,
    minContains: Option[Int] = None,
    maxContains: Option[Int] = None,
    additionalItems: OValidatingField[Schema, ItemValidator] = None // V2019_09
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
  def effectivePrefixSize: Int = 0 /*{
    prefixItems.map(_.value).orElse(v2019Items).map(_.size).getOrElse(0)
  }*/
}

object ArrayRestriction {
  private implicit def eitherSchemaOrArrayCodec: Codec[Either[Schema, Vector[Schema]]] = {
    Codecs.disjunctEitherCodec[Schema, Vector[Schema]]
  }

  implicit lazy val codec: Codec.AsObject[ArrayRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  case object TypeOrTypesValidator
  implicit val itemsValidatorProvider: ValidationProvider[
    (ValidatingField[Either[Schema, Vector[Schema]], TypeOrTypesValidator.type], ArrayRestriction)
  ] =
    ValidationProvider.forFieldWithContext { (origin, value, context) =>
      value match {
        case Left(value)  => ItemValidator(origin, value, context.effectivePrefixSize)
        case Right(value) => PrefixValidator(origin, value)
      }
    }

  /*
  implicit val containsValidationProvider: ValidationProvider[
    (ValidatingField[Schema, ContainsValidator], ArrayRestriction)
  ] = ValidationProvider.forFieldWithContext { (origin, value, context) =>
    val minContains = context.minContains.getOrElse(1)
    val maxContains = context.maxContains
    ContainsValidator(value.validator(origin), minContains, maxContains)
  }

   */

  implicit val validationProvider: ValidationProvider[ArrayRestriction] =
    ValidationProvider.visitingSequental[ArrayRestriction]

  /*

  implicit val validationProvider: ValidationProvider[ArrayRestriction] = ValidationProvider.withOrigin {
    (origin, restriction) =>
      Validator.sequenceOfOpts(
        restriction.v2020Items.map { schema =>
          origin.validatorFor("items")(
            ItemValidator(_, schema, restriction.effectivePrefixSize)
          )
        },
        restriction.v2019Items.map { items =>
          // V2019
          origin.validatorFor("items")(
            PrefixValidator(_, items)
          )
        },
        for {
          additionalItems <- restriction.additionalItems
          if restriction.v2019Items.isDefined
        } yield {
          // V2019, additionalItems is only used if items is given
          origin.validatorFor("additionalItems") {
            ItemValidator(_, additionalItems, restriction.effectivePrefixSize)
          }
        },
        restriction.minItems.map { minItems =>
          SimpleValidator.MinItems(minItems)
        },
        restriction.maxItems.map { maxItems =>
          SimpleValidator.MaxItems(maxItems)
        },
        restriction.uniqueItems.filter(_ == true).map { unique =>
          SimpleValidator.Unique
        },
        restriction.prefixItems.map { prefixItems =>
          origin.validatorFor("prefixItems") {
            PrefixValidator(_, prefixItems)
          }
        },
        restriction.contains.map { contains =>
          val containSchema = contains.validator(origin)
          val minContains = restriction.minContains.getOrElse(1)
          val maxContains = restriction.maxContains
          ContainsValidator(containSchema, minContains, maxContains)
        },
        restriction.unevaluatedItems.map { schema =>
          val validator = schema.validator(origin)
          UnevaluatedItemsValidator(validator)
        }
      )
  }
   */
}
