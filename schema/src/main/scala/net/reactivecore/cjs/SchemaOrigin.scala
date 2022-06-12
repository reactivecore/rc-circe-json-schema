package net.reactivecore.cjs

import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.validator.Validator

/** Context which led to this schema */
case class SchemaOrigin(
    parentId: RefUri,
    path: JsonPointer,
    metaSchema: Option[Schema]
) {

  /** Returns the context for entering an Array Object. */
  def enterObject(name: String): SchemaOrigin = {
    copy(
      path = path.enterObject(name)
    )
  }

  /** Returns the context for entering an Array. */
  def enterArray(idx: Int): SchemaOrigin = {
    copy(
      path = path.enterArray(idx)
    )
  }

  /**
    * Helper for building validators.
    * Entering a keyword, increasing path.
    *
    * If the word is not in the vocabulary, it should be ignored.
    */
  def validatorFor(name: String)(f: SchemaOrigin => Validator): Validator = {
    // TODO: Check vocabulary
    f(enterObject(name))
  }

  /**
    * Helper for building validators.
    * Entering a keyword, increasing path.
    */
  def validatorForIdx(idx: Int)(f: SchemaOrigin => Validator): Validator = {
    f(enterArray(idx))
  }
}
