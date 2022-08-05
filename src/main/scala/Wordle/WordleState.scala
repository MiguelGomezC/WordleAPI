package Wordle

case class WordleState(table: Table,
                       hiddenWord: String) {
  def showcase: Unit =
    println(table, hiddenWord)
}

object WordleState {

  def initial(hiddenWordc: String): WordleState = {
    /* Set initial state */
    WordleState(
      table = List.empty,
      hiddenWord = hiddenWordc
    )
  }
}