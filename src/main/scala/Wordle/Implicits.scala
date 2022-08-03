package Wordle

import scala.io.Source
import scala.util.Using


trait Implicits {

  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map((char: Char) => Letter(char, Black))
  }

  implicit class greenCheck(letter: Letter) {
    def isGreen: Boolean = letter.color == Green
    def whoseCharIs(charac: Char): Boolean = letter.c == charac
  }

  implicit val wordleDictionary: List[String] =
    Using(Source.fromResource("es.txt")){ source =>
      source.getLines.toList
    }.getOrElse(List())

  implicit val defaultHiddenWord: Word =
    "LUMEN".toBlackWord

}
