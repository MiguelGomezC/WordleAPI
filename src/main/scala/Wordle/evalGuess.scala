package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}

object evalGuess {

  def colorItGreen(target: String, reference: String): Word =
    target.zip(reference)
      .map {case (t,r) => Letter(t, if (t==r) {
                                    Green} else Black)
      }

  def initial(responseStr: String, candidateStr: String): BagWordState =
    BagWordState(responseStr.toBlackWord, candidateStr.toBlackWord, Map.empty)

  def evalGuess(responseStr: String, candidateStr: String): Word = {
    evalGuessState.runA(initial(responseStr, candidateStr)).value
  }

  def evalGuessState: State[BagWordState, Word] = {
    for {
      hiddenWordGreenColored <- inspect[BagWordState, Word] { case BagWordState(hiddenWord, candidateWord, _) =>
        colorItGreen(hiddenWord.toString, candidateWord.toString)
      }
      candidateGreenColored <- inspect[BagWordState,Word] { case BagWordState(hiddenWord, candidateWord, _) =>
        colorItGreen(candidateWord.toString, hiddenWord.toString)
      }
      _ <- modify { bag: BagWordState =>
        bag.copy(
          hiddenWord = hiddenWordGreenColored.filter(!_.isGreen))
      }
      remainingOccurrencesToColor <- inspect[BagWordState, Map[Char,Int]]{ case BagWordState(hiddenWithoutGreen, _, _) =>
        hiddenWithoutGreen.toSet
          .map((letter: Letter) => (letter.c, hiddenWithoutGreen.count(_.whoseCharIs(letter.c)))).toMap
      }
    } yield candidateGreenColored.colorItYellow(remainingOccurrencesToColor)
  }
}
