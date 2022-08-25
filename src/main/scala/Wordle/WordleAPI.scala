package Wordle

case class WordleAPI(responseStr: String, dictionary: List[String], attempts: Table){

  def makeTurn(safeCandidateWord: Word): WordleAPI = {
    this.copy(attempts = safeCandidateWord :: attempts)
  }

  def showTable: String = this.attempts.reverse.zipWithIndex.map {
    case (element, index) => s"${(index+1).toString}: ${element.prettyString}"
  }.mkString(sep = "\n")

  def isFinished: Boolean =
    attempts.length >= 6 || attempts.headOption.exists(_.guessed)
}

object WordleAPI {

  def apply(responseStr: String, dictionary: List[String]): WordleAPI = {
    /* initial WordleAPI from hiddenWord */
    WordleAPI(responseStr.toLowerCase, dictionary, List.empty)
  }
}
