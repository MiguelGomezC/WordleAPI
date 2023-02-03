package Wordle

import scala.util.Random

case class Status(responseStr: String, dictionary: List[String], candidateWord: Word, attempts: Int) {

  def next(candidateStr: String): Status = {
    this.copy(candidateWord = Next.evalGuess.evalGuess(this.responseStr, candidateStr),
      attempts = attempts + 1)
  }

  def validLength(candidateStr: String): Boolean =
    this.responseStr.length != candidateStr.length

  def knownWord(candidateStr: String): Boolean =
    !this.dictionary.contains(candidateStr)

  def isFinished: Boolean =
    attempts >= 6 || candidateWord.guessed
}

object Status {

  def apply(dictionary: List[String], capSize: Int = 8): Status = {
    /* set hidden word randomly */
    val random = new Random
    val dict: List[String] = dictionary.filter(_.length<capSize)
    val hiddenStr: String = dict(
      random.nextInt(dict.length)
    )
    Status(hiddenStr.toLowerCase, dict, "".toBlackWord, 0)
  }
}