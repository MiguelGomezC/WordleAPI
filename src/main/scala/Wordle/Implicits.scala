package Wordle

import cats.data.State
import cats.implicits._
import scala.io.Source
import scala.util.Using


trait Implicits {

  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map((char: Char) => Letter(char, Black))
  }

  implicit class WordUtils(word: Word) {
    override def toString: String =
      word.map(_.c).toString
    def showcaseHidden: Word =
      word.map{_ => Letter('*', Black) }
    def guessed: Boolean =
        word.forall(_.color == Green)
  }

  implicit class greenCheck(letter: Letter) {
    def isGreen: Boolean = letter.color == Green
    def whoseCharIs(charac: Char): Boolean = letter.c == charac
    def whoseColorIs(color: Color): Boolean = letter.color == color
  }

  implicit val wordleDictionary: List[String] =
    Using(Source.fromResource("es.txt")){ source =>
      source.getLines.toList
    }.getOrElse(List())

  implicit val defaultHiddenWord: Word =
    "LUMEN".toBlackWord

}
