package net.reactivecore.cjs

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TestBase extends AnyFlatSpec with Matchers with EitherExt.ImplicitsForTest {

  def ensureJsonEqual(left: Json, right: Json): Unit = {
    if (left != right) {
      println(s"Difference: ${findDifference(left, right)}")
    }
    left shouldBe right
  }

  def findDifference(left: Json, right: Json, path: List[String] = Nil): Option[String] = {
    (left, right) match {
      case (l, r) if l.isObject && r.isObject =>
        val lo = l.asObject.get
        val ro = r.asObject.get
        val keys = (lo.keys ++ ro.keys).toSet
        val it = keys.iterator
        while (it.hasNext) {
          val key = it.next()
          val leftValue = lo.apply(key)
          val rightValue = ro.apply(key)
          if (leftValue.isEmpty) {
            return Some(s"Missing on left: ${key}, path=${path}")
          }
          if (rightValue.isEmpty) {
            return Some(s"Missing on right: ${key}, path=${path}")
          }
          findDifference(leftValue.get, rightValue.get, key :: path) match {
            case Some(diff) => return Some(diff)
            case None       => // ok
          }
        }
        None
      case (l, r) if l == r => None
      case (l, r)           => Some(s"Left=${left}, Right=${right} in ${path}")
    }
  }
}
