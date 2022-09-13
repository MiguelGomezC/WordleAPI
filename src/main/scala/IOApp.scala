import Wordle._
import cats.effect.IO
import cats.implicits.catsSyntaxMonadIdOps

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

  implicit class showcaseTries(tries: List[Word]) {
    def showTable: String = tries.zipWithIndex.map {
      case (element, index) => s"${(index+1).toString}: ${element.prettyString}"
    }.mkString(sep = "\n")
  }

  def console(machine: WordleAPI, tries: List[Word]): IO[(WordleAPI, List[Word])] = {
    putStrLn(s"Hidden word: ${machine.responseStr.toBlackWord.showcaseHidden.prettyString}" +
      s". Length = ${machine.responseStr.length}").flatMap(_ =>
      putStrLn("-Please, provide a new guess: ")).flatMap(_ =>
      readLn.flatMap((candidateStr: String) =>
        IO(machine.takeTurn(candidateStr)).flatMap(comparison =>
          comparison.fold(
            (e: Error) => {
              putStrLn(e match {
                case LengthMismatch => "Lenghts do not match!".prettyMessage
                case WordNotFound => "Word is not contained in the dictionary!".prettyMessage
              }).flatMap(_ =>
                putStrLn(tries.showTable).flatMap(_ =>
                  console(machine, tries)))
            },
            (m: WordleAPI) => {
              putStrLn("Provided word was valid.".prettyMessage).flatMap(_ =>
                IO.pure(tries:+m.candidateWord).flatMap(list =>
                  putStrLn(list.showTable).flatMap(_ =>
                  IO.pure((m, list)))))
            }
          ))))
  }

  def run(machine: WordleAPI): IO[Unit] = {
    (machine, List.empty[Word]).iterateWhileM{
      case (wordleAPI, tries) => console(wordleAPI, tries)}{
      case (wordleAPI, _) => !wordleAPI.isFinished
    }.flatMap{case(m,_) => putStrLn(s"Game is over. Target word was ${m.responseStr.prettyMessage}")}
  }
}
