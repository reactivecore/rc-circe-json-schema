package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import net.reactivecore.cjs.validator.array._
import net.reactivecore.cjs.Schema

case class ArrayRestriction(
    items: Option[Schema] = None,
    contains: Option[Schema] = None,
    minItems: Option[Long] = None,
    maxItems: Option[Long] = None,
    uniqueItems: Option[Boolean] = None,
    unevaluatedItems: Option[Schema] = None,
    prefixItems: Option[Vector[Schema]] = None,
    minContains: Option[Int] = None,
    maxContains: Option[Int] = None
)

object ArrayRestriction {
  implicit lazy val codec: Codec.AsObject[ArrayRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit val validationProvider: ValidationProvider[ArrayRestriction] = ValidationProvider.withUri {
    (parentId, path, restriction) =>
      Validator.sequenceOfOpts(
        restriction.items.map { schema =>
          val schemaValidator = schema.validator(parentId, path)
          val prefixSize = restriction.prefixItems.map(_.size).getOrElse(0)
          ItemValidator(schemaValidator, prefixSize)
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
          val subPath = path.enterObject("prefixItems")
          val prefixValidators = prefixItems.zipWithIndex.map { case (schema, idx) =>
            schema.validator(parentId, subPath.enterArray(idx))
          }
          PrefixValdiator(prefixValidators)
        },
        restriction.contains.map { contains =>
          val containSchema = contains.validator(parentId, path)
          val minContains = restriction.minContains.getOrElse(1)
          val maxContains = restriction.maxContains
          ContainsValidator(containSchema, minContains, maxContains)
        },
        restriction.unevaluatedItems.map { schema =>
          val validator = schema.validator(parentId, path)
          UnevaluatedItemsValidator(validator)
        }
      )
  }
}
