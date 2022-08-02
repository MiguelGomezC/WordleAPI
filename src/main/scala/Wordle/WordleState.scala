package Wordle

case class WordleState(table: Table,
                       dict: Map[String, String],
                       hiddenWord: String)

object WordleState {

  def apply(dictc: Map[String, String], hiddenWordc: String): WordleState = {
    /* Set initial state */
    WordleState(
      table = IndexedSeq(hiddenWordc.toBlackWord),
      dict = dictc,
      hiddenWord = hiddenWordc
    )
  }
}