package Wordle

import Wordle.WordleAPI.evalGuess
import cats.data.State
import cats.data.State.{inspect, modify}

trait WordleAPI {

  def evalGuess: (String, String) => Word

  def attempt(responseStr: String, candidateStr: String)
             (implicit dict: List[String]): Either[Exception, Word] =
    if (responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match"))
    } else {
      if (!dict.contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary"))
      } else {
        Right(evalGuess(responseStr, candidateStr))
      }
    }

  def runGame(wordc: String): State[WordleState, Word] = {
    /* usar algo como iterate aquí */
    for {
      word <- inspect((tablero: WordleState) => evalGuess(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => evalGuess(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => evalGuess(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => evalGuess(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => evalGuess(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
    } yield word
  }

}

object WordleAPI {

  def colorItGreen(target: String, reference: String): Word =
    target.zip(reference)
      .map(pair => Letter(pair._1,
        if (pair._1 == pair._2) {
          Green
        } else Black)
      )

  def evalGuessState: State[BagWordState, Word] = {
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
    } yield candidateGreenColored.colorItYellow(remainingOccurrencesToColor)
  }

  def evalGuess(responseStr: String, candidateStr: String): Word = {
    evalGuessState.runA(BagWordState(responseStr.toBlackWord, candidateStr.toBlackWord, Map.empty)).value
  }
}
