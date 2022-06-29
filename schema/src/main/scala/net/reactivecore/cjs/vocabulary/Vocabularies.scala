package net.reactivecore.cjs.vocabulary

/** Holds vocabularies */
case class Vocabularies(
    vocabularies: Seq[Vocabulary]
) {}

object Vocabularies {

  def default2020: Vocabularies = Vocabularies(
    Seq(CoreVocabulary2020)
  )
}
