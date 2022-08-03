import Wordle._
import cats.data.State.{inspect, modify}
import cats.data.State

object evalGuess {

  def colorItGreen(target: String, reference: String): Word =
    target.zip(reference)
      .map(pair => Letter(pair._1,
        if (pair._1 == pair._2) {
          Green
        } else Black)
      )

  def evalGuessState: State[BagWordState, (Map[Char, Int], Word)] = {
    for {
      hiddenWordGreenColored <- inspect { case BagWordState(hiddenWord, candidateWord, _) =>
        colorItGreen(hiddenWord.toString, candidateWord.toString)
      }
      candidateGreenColored <- inspect { case BagWordState(hiddenWord, candidateWord, _) =>
        colorItGreen(candidateWord.toString, hiddenWord.toString)
      }
      _ <- modify { bag: BagWordState =>
        bag.copy(
          hiddenWord = hiddenWordGreenColored.filter(!_.isGreen))
      }
      remainingOccurrencesToColor <- inspect { case BagWordState(hiddenWithoutGreen, _, _) =>
        hiddenWithoutGreen.toSet
          .map(letter => (letter.c, hiddenWithoutGreen.count(_.whoseCharIs(letter.c)))).toMap
      }
    } yield (remainingOccurrencesToColor, candidateGreenColored)
  }
}
