package Wordle

import Machine._
import cats.Applicative

import scala.util.Random

trait WordleAPI extends Machine[Either[Error,_]] {
  type Q = Status
  val current: Q
  val wordle: AbstractWordleButtons[Either[Error, _], Q]
}

object WordleAPI {

  def apply(dictionary: List[String]): WordleAPI = new WordleAPI {
    val current: Status = Status.apply(dictionary)
    val wordle: AbstractWordleButtons[Either[Error, _], Status] =
      AbstractWordleButtons[Either[Error, _], Status](Status(dictionary),
        str => status => status.next(str),
        status => status.candidateWord,
        status => status.attempts)
  }
}



/*
case class WordleAPI(responseStr: String, dictionary: List[String], candidateWord: Word, attempts: Int) {

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
        Right(this.makeTurn(Next.evalGuess.evalGuess(this.responseStr, candidateStr)))
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
*/