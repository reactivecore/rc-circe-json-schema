package net.reactivecore.cjs.validator

import net.reactivecore.cjs.resolver.RefUri

import scala.collection.immutable.BitSet

/** Stackable state. */
case class StackElement(
    id: RefUri,
    dynamicAnchors: Map[String, Validator] = Map.empty
)

/** Evaluation State for prcessing validation tasks like unevaluatedProperties */
case class ValidationState(
    evaluatedProperties: Set[String],
    evaluatedIndices: BitSet,
    stack: List[StackElement]
) {
  def mergeEvaluated(other: ValidationState): ValidationState = {
    copy(
      evaluatedProperties = evaluatedProperties ++ other.evaluatedProperties,
      evaluatedIndices = evaluatedIndices ++ other.evaluatedIndices
    )
  }

  def withoutEvaluated: ValidationState = {
    copy(
      evaluatedProperties = Set.empty,
      evaluatedIndices = BitSet.empty
    )
  }

  def pushState(id: RefUri): ValidationState = {
    copy(
      stack = StackElement(id) :: stack
    )
  }

  def popState(): ValidationState = {
    copy(
      stack = stack.tail
    )
  }

  def withDynamicAnchor(anchor: String, validator: Validator): ValidationState = {
    stack match {
      case Nil => this // can this happen?
      case head :: tail =>
        if (head.dynamicAnchors.contains(anchor)) {
          this
        } else {
          val updated = head.copy(
            dynamicAnchors = head.dynamicAnchors + (anchor -> validator)
          )
          val updatedStack = updated :: tail
          copy(
            stack = updatedStack
          )
        }
    }
  }
}

object ValidationState {
  val empty: ValidationState = ValidationState(Set.empty, BitSet.empty, List(StackElement(RefUri())))
}
