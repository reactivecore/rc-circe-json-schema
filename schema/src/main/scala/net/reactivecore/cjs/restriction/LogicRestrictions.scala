package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator._
import net.reactivecore.cjs.{Schema, SchemaOrigin}

/** Further restrictions of a type. */
case class LogicRestrictions(
    oneOf: Option[Vector[Schema]] = None,
    anyOf: Option[Vector[Schema]] = None,
    not: Option[Schema] = None,
    allOf: Option[Vector[Schema]] = None,
    `if`: Option[Schema] = None,
    `then`: Option[Schema] = None,
    `else`: Option[Schema] = None
)

object LogicRestrictions {
  implicit lazy val codec: Codec.AsObject[LogicRestrictions] = Codecs.withoutNulls(semiauto.deriveCodec[LogicRestrictions])

  implicit lazy val validationProvider: ValidationProvider[LogicRestrictions] = ValidationProvider.withOrigin {
    (context, restrictions) =>
      allOf(
        logicChain(context.enterObject("oneOf"), restrictions.oneOf, x => OneOfValidator(x.toVector)),
        logicChain(context.enterObject("anyOf"), restrictions.anyOf, x => AnyOfValidator(x.toVector)),
        logicChain(context.enterObject("allOf"), restrictions.allOf, x => AllOfValidator(x.toVector)),
        restrictions.not
          .map { schema =>
            NotValidator(schema.validator(context.enterObject("not")))
          }
          .getOrElse(Validator.success),
        ifThenElse(context, restrictions)
      )
  }

  /** Note: Logic operators are combined using all of and not run sequentially. */
  private def allOf(validators: Validator*): Validator = {
    val withoutTrue = validators.filterNot(_ == Validator.success)
    withoutTrue.size match {
      case 0 => Validator.success
      case 1 => withoutTrue.head
      case _ => AllOfValidator(withoutTrue.toVector)
    }
  }

  private def ifThenElse(origin: SchemaOrigin, logicRestrictions: LogicRestrictions): Validator = {
    val got = for {
      i <- logicRestrictions.`if`
    } yield {
      val ic = i.validator(origin.enterObject("if"))
      val t = logicRestrictions.`then`.map(_.validator(origin.enterObject("then")))
      val e = logicRestrictions.`else`.map(_.validator(origin.enterObject("else")))
      IfThenElseValidator(ic, t, e)
    }
    got.getOrElse(Validator.success)
  }

  private def logicChain(
      origin: SchemaOrigin,
      input: Option[Vector[Schema]],
      combiner: Seq[Validator] => Validator
  ): Validator = {
    input match {
      case None => Validator.success
      case Some(schemas) =>
        val validators = schemas.zipWithIndex.map { case (schema, idx) =>
          schema.validator(origin.enterArray(idx))
        }
        combiner(validators)
    }
  }
}
