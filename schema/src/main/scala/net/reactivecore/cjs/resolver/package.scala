package net.reactivecore.cjs

package object resolver {
  type SimpleResolveResult[T] = Either[ResolveError, T]
}
