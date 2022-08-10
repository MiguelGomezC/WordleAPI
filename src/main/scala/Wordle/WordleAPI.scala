package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.effect.IO
import Wordle.evalGuess._
import cats.effect.unsafe.implicits.global

case class WordleAPI(responseStr: String, attempts: Table){

  def makeTurn(safeCandidateWord: Word): WordleAPI = {
    WordleAPI(
      responseStr,
      safeCandidateWord :: attempts
    )
  }

  def isFinished: Boolean =
    attempts.length >= 6 || attempts.headOption.exists(_.guessed)

}
object WordleAPI {

  def attemptComparison(responseStr: String, candidateStr: String)
             (implicit dict: List[String]): Either[Exception, Word] =
    if (responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match"))
    } else {
      if (!dict.contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary"))
      } else {
        Right(evalGuess(responseStr, candidateStr))
      }
    }

  /* @tailrec */
  def consoleAttempt(responseStr: String): IO[Word] = {
    putStrLn(s"Hidden word: ${responseStr.toBlackWord.showcaseHidden}. \n" +
        "Please, provide a new guess: ").flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(attemptComparison(responseStr, candidateStr)).flatMap(comparison =>
        comparison.fold[IO[Word]](
          (e:Exception) => {
          putStrLn(e.toString)
            .flatMap(_ => consoleAttempt(responseStr))
        },
          (w: Word) => {
            putStrLn(s"Provided word was valid. The result is: $w").flatMap(_ => IO.pure(w))
          }
        ))))
  }

  /*
  def retry[T](op: => Try[T])(onWrong: Throwable => Any): T =
    Iterator.continually(op).flatMap {
      case Success(t) => Some(t)
      case Failure(f) => onWrong(f); None
    }.toSeq.head
   */

  def run(responseStr: String): Unit = {
    var state: WordleAPI = WordleAPI(responseStr, List.empty)
    while (!state.isFinished) {
      state = state.makeTurn(consoleAttempt(responseStr).unsafeRunSync)
    }
  }
  /*
  def runGame(wordc: String): State[WordleState, Word] = {
    for {
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
    } yield word
  }
   */
}
