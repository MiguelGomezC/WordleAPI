package Wordle

import scala.util.Random

case class WordleAPI(responseStr: String, dictionary: List[String], attempts: Table){

  def makeTurn(safeCandidateWord: Word): WordleAPI = {
    this.copy(attempts = safeCandidateWord :: attempts)
  }

  def takeTurn(candidateStr: String): Either[Exception, WordleAPI] = {
    if (this.responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match!"))
    } else {
      if (!this.dictionary.contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary!"))
      } else {
        Right(this.makeTurn(evalGuess(this.responseStr, candidateStr)))
      }
    }
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


  def apply(dictionary: List[String], capSize: Int = 8): WordleAPI = {
    /* set hidden word randomly */
    val random = new Random
    val dict: List[String] = dictionary.filter(_.length<capSize)
    val hiddenStr: String = dict(
      random.nextInt(dict.length)
    )
    WordleAPI(hiddenStr.toLowerCase, dict, List.empty)
  }
}
