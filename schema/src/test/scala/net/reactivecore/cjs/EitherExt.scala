package net.reactivecore.cjs

import org.scalatest.Assertions

class EitherExt[L, R](underlying: Either[L, R], fail: String => Nothing) {
  def forceRight: R = {
    underlying match {
      case Left(value) => fail(s"Expected right, got left ${value}")
      case Right(ok)   => ok
    }
  }
}

object EitherExt {
  trait ImplicitsDefault {
    import scala.language.implicitConversions
    implicit def toEitherExt[L, R](in: Either[L, R]): EitherExt[L, R] =
      new EitherExt(in, msg => throw new RuntimeException(msg))
  }

  trait ImplicitsForTest {
    self: Assertions =>
    import scala.language.implicitConversions

    implicit def toEitherExt[L, R](in: Either[L, R]): EitherExt[L, R] = new EitherExt(in, msg => fail(msg))
  }
}
