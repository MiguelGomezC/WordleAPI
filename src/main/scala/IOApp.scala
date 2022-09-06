import Wordle._
import cats.effect.IO

object IOApp {

  /* IO related */

  def putStrLn(value: String): IO[Unit] = IO(println(value))
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  implicit class redMessage(str: String) {
    def prettyMessage: String = {
      val ansiRed = "\u001B[31m"
      val ansiReset = "\u001B[0m"
      ansiRed + str + ansiReset
    }
  }

  def console(machine: WordleAPI): IO[WordleAPI] = {
    putStrLn(s"Hidden word: ${machine.responseStr.toBlackWord.showcaseHidden.prettyString}" +
      s". Length = ${machine.responseStr.length}").flatMap(_ =>
      putStrLn("-Please, provide a new guess: ")).flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(machine.takeTurn(candidateStr)).flatMap(comparison =>
          comparison.fold[IO[WordleAPI]](
            (e: Exception) => {
              putStrLn(e.getMessage.prettyMessage).flatMap(_ =>
                putStrLn(machine.showTable).flatMap(_ =>
                  console(machine)))
            },
            (m: WordleAPI) => {
              putStrLn("Provided word was valid.".prettyMessage).flatMap(_ =>
                putStrLn(m.showTable).flatMap(_ =>
                  IO.pure(m)))
            }
          ))))
  }

  def run(machine: WordleAPI): IO[Unit] = {
    whileIO(
      machine)(
      wordleAPI => IO.pure(!wordleAPI.isFinished))(
      wordleAPI => console(wordleAPI)
    ).flatMap(m => putStrLn(s"Game is over. Target word was ${m.responseStr.prettyMessage}"))
  }
}
