package Wordle

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

  def apply(responseStr: String): WordleAPI = {
    /* initial WordleAPI from hiddenWord */
    WordleAPI(responseStr.toLowerCase, List.empty)
  }

  def attemptComparison(responseStr: String, candidateStr: String)
             (implicit dict: List[String]): Either[Exception, Word] =
    if (responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match!"))
    } else {
      if (!dict.contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary!"))
      } else {
        Right(evalGuess(responseStr, candidateStr))
      }
    }

  /* @tailrec */
  def consoleAttempt(responseStr: String): IO[Word] = {
    putStrLn(s"Hidden word: ${responseStr.toBlackWord.showcaseHidden.prettyString}. \n" +
        "Please, provide a new guess: ").flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(attemptComparison(responseStr, candidateStr)).flatMap(comparison =>
        comparison.fold[IO[Word]](
          (e:Exception) => {
          putStrLn(e.toString)
            .flatMap(_ => consoleAttempt(responseStr))
        },
          (w: Word) => {
            putStrLn(s"Provided word was valid. The result is: ${w.prettyString}").flatMap(_ => IO.pure(w))
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
    var state: WordleAPI = WordleAPI(responseStr)
    while (!state.isFinished) {
      //IO(state.attempts.flatMap(row => putStrLn(row.prettyString))).unsafeRunSync
      state = state.makeTurn(consoleAttempt(responseStr).unsafeRunSync)
    }
  }
}
