package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.implicits._

object evalGuess {

  def initial(responseStr: String, candidateStr: String): BagWordState =
    BagWordState(responseStr.toBlackWord, candidateStr.toBlackWord, Map.empty)

  def evalGuess(responseStr: String, candidateStr: String): Word = {
    evalGuessState.runA(initial(responseStr, candidateStr)).value
  }

  def evalGuessState: State[BagWordState, Word] = {
    for {
      hiddenWordInit <- inspect[BagWordState, Word]{bagWordState => bagWordState.hiddenWord}
      candidateWordInit <- inspect[BagWordState, Word]{bagWordState => bagWordState.candidateWord}
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
      }.inspect(_.hiddenWord)
      candidateYellowColored <- candidateGreenColored.toList.traverse {
        (letter: Letter) => modify[BagWordState]{
          case BagWordState(hiddenWord: Word, candidateWord: Word, bag: Map[Char, Int]) =>
            val isGreen = !letter.color.equals(Green)
            BagWordState(
              hiddenWord,
              candidateWord
                .mapAs(_ :+ letter)
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ :+ letter.copy(color = Yellow)),
              bag
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ => bag.modify(letter.c)(_-1))
            )
        }
      }.inspect(_.candidateWord)
    } yield candidateYellowColored
  }
}
