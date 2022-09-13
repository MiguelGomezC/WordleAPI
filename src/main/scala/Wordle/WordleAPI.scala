package Wordle

import scala.util.Random

case class WordleAPI(responseStr: String, dictionary: List[String], candidateWord: Word, attempts: Int){

  def makeTurn(safeCandidateWord: Word): WordleAPI = {
    this.copy(candidateWord = safeCandidateWord, attempts = attempts + 1)
  }

  def takeTurn(candidateStr: String): Either[Error, WordleAPI] = {
    if (this.responseStr.length != candidateStr.length) {
      Left(LengthMismatch)
    } else {
      if (!this.dictionary.contains(candidateStr)){
        Left(WordNotFound)
      } else {
        Right(this.makeTurn(Wordle.evalGuess.evalGuess(this.responseStr, candidateStr)))
      }
    }
  }

  def isFinished: Boolean =
    attempts >= 6 || candidateWord.guessed
}

object WordleAPI {

  def apply(responseStr: String, dictionary: List[String]): WordleAPI = {
    /* initial WordleAPI from hiddenWord */
    WordleAPI(responseStr.toLowerCase, dictionary, "".toBlackWord, 0)
  }

  def apply(dictionary: List[String], capSize: Int = 8): WordleAPI = {
    /* set hidden word randomly */
    val random = new Random
    val dict: List[String] = dictionary.filter(_.length<capSize)
    val hiddenStr: String = dict(
      random.nextInt(dict.length)
    )
    WordleAPI(hiddenStr.toLowerCase, dict, "".toBlackWord, 0)
  }
}
