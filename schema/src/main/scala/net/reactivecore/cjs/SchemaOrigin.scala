package net.reactivecore.cjs

import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}

/** Context which led to this schema */
case class SchemaOrigin(
    parentId: RefUri,
    path: JsonPointer
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
}
