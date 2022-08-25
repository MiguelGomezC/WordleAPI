package Wordle

import cats.effect.IO

trait IOApp {

  /* IO related */

  def putStrLn(value: String): IO[Unit] = IO(println(value))
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  def attemptComparison(candidateStr: String)(machine: WordleAPI): Either[Exception, Word] = {
    if (machine.responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match!"))
    } else {
      if (!machine.dictionary.contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary!"))
      } else {
        Right(evalGuess(machine.responseStr, candidateStr))
      }
    }
  }

  def consoleAttempt(machine: WordleAPI): IO[Word] = {
    putStrLn(s"Hidden word: ${machine.responseStr.toBlackWord.showcaseHidden.prettyString}. \n" +
      "Please, provide a new guess: ").flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(attemptComparison(candidateStr)(machine)).flatMap(comparison =>
          comparison.fold[IO[Word]](
            (e:Exception) => {
              putStrLn(e.toString)
                .flatMap(_ => consoleAttempt(machine))
            },
            (w: Word) => {
              putStrLn(s"Provided word was valid. The result is: ${w.prettyString}").flatMap(_ => IO.pure(w))
            }
          ))))
  }

  def run(machine: WordleAPI): IO[Unit] = {
    whileIO(
      machine)(
      wordleAPI => putStrLn(wordleAPI.showTable).flatMap(_ =>IO.pure(!wordleAPI.isFinished)))(
      wordleAPI =>
        consoleAttempt(machine).flatMap(word => IO.pure(wordleAPI.makeTurn(word)))
    ).flatMap(_ => putStrLn("Game is over. Target word was " + machine.responseStr))
  }
}
