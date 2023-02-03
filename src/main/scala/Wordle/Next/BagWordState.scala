package Wordle.Next
import Wordle._

case class BagWordState(candidateWord: Word,
                        bag: Map[Char, Int]) {
  def withEmptyWord: BagWordState = this.copy(candidateWord = "".toBlackWord)
}
