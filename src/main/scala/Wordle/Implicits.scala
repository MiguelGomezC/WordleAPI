package Wordle

import cats.data.State
import cats.implicits._
import scala.io.Source
import scala.util.Using


trait Implicits {

  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map((char: Char) => Letter(char, Black))
  }

  implicit class YellowColoredWord(wordc: Word) {
    def colorItYellow(occurrencesToColor: Map[Char, Int]): Word = {
      wordc.toList.traverse {
        (letter: Letter) => State.modify[TagState]{
          case TagState(tagged: Word, bag: Map[Char, Int]) =>
            val isGreen = !letter.color.equals(Green)
            TagState(
              tagged
                .mapAs(_ :+ letter)
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ :+ letter.copy(color = Yellow)),
              bag
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ => bag.modify(letter.c)(_-1))
            )
        }
      }.inspect(_.tagged).runA(TagState("".toBlackWord, occurrencesToColor)).value
    }
  }

  implicit class WordUtils(word: Word) {
    override def toString: String = word.map(_.c).toString
    def showcaseHidden: Unit = println(word.map{_ => Letter('*', Black) })

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
