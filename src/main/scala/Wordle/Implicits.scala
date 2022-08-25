package Wordle

trait Implicits {

  implicit class VecFromWord(word: Word) {
    def toVec: Vector[(Char, Color)] = word.map{case Letter(char, color) => (char,color)}.toVector
  }

  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map(Letter(_, Black))
  }

  implicit class WordUtils(word: Word) {
    def prettyString: String = {
      val ansiGreen = "\u001B[32m"
      val ansiYellow = "\u001B[33m"
      val ansiReset = "\u001B[0m"
      word.toList.flatMap{ (letter: Letter) =>
        letter match {
          case Letter(c, Green) => ansiGreen + c.toString + ansiReset
          case Letter(c, Yellow) => ansiYellow + c.toString + ansiReset
          case Letter(c, Black) => c.toString
        }
      }.mkString
    }
    def showcaseHidden: Word =
      word.map(_ => Letter('*', Black))
    def guessed: Boolean =
        word.forall(_.color == Green)
  }

  implicit class greenCheck(letter: Letter) {
    def isGreen: Boolean = letter.color == Green
    def whoseCharIs(char: Char): Boolean = letter.c == char
    def whoseColorIs(color: Color): Boolean = letter.color == color
  }
}
