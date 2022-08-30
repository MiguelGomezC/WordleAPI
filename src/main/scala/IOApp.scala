import Wordle._
import cats.effect.IO

object IOApp {

  /* IO related */

  def putStrLn(value: String): IO[Unit] = IO(println(value))
  val readLn: IO[String] = IO(scala.io.StdIn.readLine())

  def console(machine: WordleAPI): IO[WordleAPI] = {
    putStrLn(s"Hidden word: ${machine.responseStr.toBlackWord.showcaseHidden.prettyString}" +
      s". Length = ${machine.responseStr.length} \n Please, provide a new guess: ").flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(machine.takeTurn(candidateStr)).flatMap(comparison =>
          comparison.fold[IO[WordleAPI]](
            (e: Exception) => {
              putStrLn(e.getMessage)
                .flatMap(_ => console(machine))
            },
            (m: WordleAPI) => {
              putStrLn(s"Provided word was valid. The result is: ${m.attempts.head.prettyString}")
                .flatMap(_ => IO.pure(m))
            }
          ))))
  }

  def run(machine: WordleAPI): IO[Unit] = {
    whileIO(
      machine)(
      wordleAPI => IO.pure(!wordleAPI.isFinished))(
      wordleAPI =>
        putStrLn(wordleAPI.showTable).flatMap(_ =>
          console(wordleAPI))
    ).flatMap(_ => putStrLn("Game is over. Target word was " + machine.responseStr))
  }
}
