package net.reactivecore.cjs

package object restriction {

  /** Optional ValidatingField. */
  type OValidatingField[T, V] = Option[ValidatingField[T, V]]
}
