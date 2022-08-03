import Wordle._
import cats.data.Reader

object evalGuess {

  def colorItGreen: Reader[(String, String), Word] =
    Reader{
      case (responseStr: String, candidateStr: String) =>
        responseStr.zip(candidateStr).map(pair => if (pair._1 == pair._2) {
          Letter(pair._1, Green)
        } else Letter(pair._1, Black))
    }

  def commonOccurrences: Reader[(String, String), Word] =
    Reader{
      case (responseStr: String, candidateStr: String) =>

    }




}