package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.implicits._

trait evalGuess {

  def initial(responseStr: String, candidateStr: String): BagWordState =
    BagWordState(responseStr.toBlackWord, candidateStr.toBlackWord, Map.empty)

  def evalGuess(responseStr: String, candidateStr: String): Word = {
    evalGuessState.runA(initial(responseStr, candidateStr)).value
  }

  def evalGuessState: State[BagWordState, Word] = {
    for {
      hiddenWordInit <- inspect[BagWordState, Word]{bagWordState => bagWordState.hiddenWord}
      candidateWordInit <- inspect[BagWordState, Word]{bagWordState => bagWordState.candidateWord}
      _ <- modify[BagWordState]{bagWordState => bagWordState.copy(hiddenWord = "".toBlackWord,
        candidateWord = "".toBlackWord)}
      candidateGreenColored <- hiddenWordInit.zip(candidateWordInit).toList.traverse {
        case (h,c) => modify[BagWordState]{
          case BagWordState(hiddenWord: Word, candidateWord: Word, bag: Map[Char, Int]) =>
            val coinciding = h.whoseCharIs(c.c)
            BagWordState(
              hiddenWord
                .mapIf(coinciding)(_ :+ h.copy(color = Green))
                .mapIf(!coinciding)(_ :+ h.copy(color = Black)),
              candidateWord
                .mapIf(coinciding)(_ :+ c.copy(color = Green))
                .mapIf(!coinciding)(_ :+ c.copy(color = Black)),
              bag
                .mapIf(!coinciding)(_ => bag.modifyOrAdd(h.c)(1, _+1))
            )
        }
      }.inspect(_.candidateWord)
      _ <- modify[BagWordState]{bagWordState => bagWordState.copy(hiddenWord = "".toBlackWord,
        candidateWord = "".toBlackWord)}
      candidateYellowColored <- candidateGreenColored.toList.traverse {
        (letter: Letter) => modify[BagWordState]{
          case BagWordState(hiddenWord: Word, candidateWord: Word, bag: Map[Char, Int]) =>
            val isGreen = letter.whoseColorIs(Green)
            val bagContainsPositive = bag.containsSuch(letter.c)(_>0)
            BagWordState(
              hiddenWord,
              candidateWord
                .mapIf(isGreen || !bagContainsPositive)(_ :+ letter)
                .mapIf(!isGreen && bagContainsPositive)(_ :+ letter.copy(color = Yellow)),
              bag
                .mapIf(!isGreen && bagContainsPositive)(_.modify(letter.c)(_-1))
            )
        }
      }.inspect(_.candidateWord)
    } yield candidateYellowColored
  }
}
