package Wordle

case class WordleState(table: Table,
                       dict: Map[String, String],
                       hiddenWord: String)

object WordleState {

  def apply(dict: Map[String, String], hiddenWordc: String): WordleState = {
    /* Set initial state */
    WordleState(
      table = IndexedSeq(hiddenWordc.toBlackWord),
      dict = dict,
      hiddenWord = hiddenWordc
    )
  }
}