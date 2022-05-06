package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import net.reactivecore.cjs.validator.array._
import net.reactivecore.cjs.{Schema, SchemaOrigin}
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}

case class ArrayRestriction(
    items: Option[Either[Schema, Vector[Schema]]] = None, // Left: V2020_12, Right: V2019_09
    contains: Option[Schema] = None,
    minItems: Option[Long] = None,
    maxItems: Option[Long] = None,
    uniqueItems: Option[Boolean] = None,
    unevaluatedItems: Option[Schema] = None,
    prefixItems: Option[Vector[Schema]] = None,
    minContains: Option[Int] = None,
    maxContains: Option[Int] = None,
    additionalItems: Option[Schema] = None // V2019_09
) {

  /** Items in V2019 Notation */
  def v2019Items: Option[Vector[Schema]] = {
    items.flatMap(_.right.toOption)
  }

  /** Items in V2020 Notation */
  def v2020Items: Option[Schema] = {
    items.flatMap(_.left.toOption)
  }

  /** Returns the number of items, the restriction defines using prefixItems or V2019 Items notation. */
  def effectivePrefixSize: Int = {
    prefixItems.orElse(v2019Items).map(_.size).getOrElse(0)
  }
}

object ArrayRestriction {
  private implicit def eitherSchemaOrArrayCodec: Codec[Either[Schema, Vector[Schema]]] =
    Codecs.disjunctEitherCodec[Schema, Vector[Schema]]

  implicit lazy val codec: Codec.AsObject[ArrayRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit val validationProvider: ValidationProvider[ArrayRestriction] = ValidationProvider.withOrigin {
    (origin, restriction) =>
      Validator.sequenceOfOpts(
        restriction.v2020Items.map { schema =>
          origin.validatorFor("items") { subOrigin =>
            ItemValidator(schema.validator(subOrigin), restriction.effectivePrefixSize)
          }
        },
        restriction.v2019Items.map { items =>
          // V2019
          prefixValidator("items", origin, items)
        },
        for {
          additionalItems <- restriction.additionalItems
          if restriction.v2019Items.isDefined
        } yield {
          // V2019, additionalItems is only used if items is given
          origin.validatorFor("additionalItems") { subOrigin =>
            ItemValidator(additionalItems.validator(subOrigin), restriction.effectivePrefixSize)
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
          prefixValidator("prefixItems", origin, prefixItems)
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

  private def prefixValidator(name: String, origin: SchemaOrigin, prefix: Vector[Schema]): Validator = {
    val subPath = origin.enterObject(name)
    val prefixValidators = prefix.zipWithIndex.map { case (schema, idx) =>
      schema.validator(subPath.enterArray(idx))
    }
    PrefixValdiator(prefixValidators)
  }
}
