package net.reactivecore

package object cjs {
  type Result[T] = Either[Failure, T]
}
