package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.implicits._

object evalGuess {

  def initialBag(candidateStr: String): BagWordState =
    BagWordState(candidateStr.toBlackWord, Map.empty)

  def evalGuess(responseStr: String, candidateStr: String): Word = {
    evalGuessState(responseStr.toBlackWord).runA(initialBag(candidateStr)).value
  }

  def colorItGreen(reference: Word, target: Word): State[BagWordState, Word] = {
    reference.zip(target).toList.traverse {
      case (h,c) => modify[BagWordState]{
        case BagWordState(candidateWord: Word, bag: Map[Char, Int]) =>
          val coinciding = h.whoseCharIs(c.c)
          BagWordState(
            candidateWord
              .mapIf(coinciding)(_ :+ c.copy(color = Green))
              .mapIf(!coinciding)(_ :+ c.copy(color = Black)),
            bag
              .mapIf(!coinciding)(_ => bag.modifyOrAdd(h.c)(1, _+1))
          )
      }
    }.inspect(_.candidateWord)
  }

  def colorItYellow(target: Word): State[BagWordState, Word] = {
    target.toList.traverse {
      (letter: Letter) => modify[BagWordState]{
        case BagWordState(candidateWord: Word, bag: Map[Char, Int]) =>
          val isGreen = letter.whoseColorIs(Green)
          val bagContainsPositive = bag.containsSuch(letter.c)(_>0)
          BagWordState(
            candidateWord
              .mapIf(isGreen || !bagContainsPositive)(_ :+ letter)
              .mapIf(!isGreen && bagContainsPositive)(_ :+ letter.copy(color = Yellow)),
            bag
              .mapIf(!isGreen && bagContainsPositive)(_.modify(letter.c)(_-1))
          )
      }
    }.inspect(_.candidateWord)
  }

  def evalGuessState(hiddenWordInit: Word): State[BagWordState, Word] = {
    for {
      candidateWordInit <- inspect[BagWordState, Word]{_.candidateWord}
      _ <- modify[BagWordState]{_.withEmptyWord}
      candidateGreenColored <- colorItGreen(hiddenWordInit, candidateWordInit)
      _ <- modify[BagWordState]{_.withEmptyWord}
      candidateYellowColored <- colorItYellow(candidateGreenColored)
    } yield candidateYellowColored
  }
}
