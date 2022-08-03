package Wordle

import cats.Monad
import cats.data.{Reader, State}
import cats.data.State.{inspect, modify}

object WordleAPI {

  def evalGuess(responseStr: String, candidateStr: String): Word ={
    /* [G] coincidentes por posición
    * [Y] Dada una letra, min(conjunto(candidata), conjunto(oculta)) - coloreadas verdes */
    // READER. igual hasta lo puedo hacer con la mónada Id porque no hay dependencias que inyectar?
    val candidateGreenColored: Word = responseStr.zip(candidateStr)
      .map(pair => Letter(pair._1,
                          if (pair._1 == pair._2) {
                            Green} else Black)
        )
    val notColoredGreen: Word = candidateGreenColored.filter(!_.isGreen)
    val remainingOccurrencesToColor = notColoredGreen.toSet
      .map(letter => (letter.c, notColoredGreen.count(_.whoseCharIs(letter.c)))).toMap
    val candidateYellowColored: Word = candidateGreenColored.colorItYellow(remainingOccurrencesToColor)

    println(candidateYellowColored)
    candidateYellowColored
  }

  def evalGuessReader: Reader[(String, String) ,Word] = {
    for {
      wordGreenColored <- Reader((responseStr, candidateStr: (String, String)) =>
      responseStr.zip(candidateStr).map(pair => if (pair._1 == pair._2) {
          Letter(pair._1, Green)
        } else Letter(pair._1, Black))
      )
    } yield
  }

  def attempt(responseStr: String, candidateStr: String): Either[Exception, Word] =
    if (responseStr.length != candidateStr.length) {
      Left(new Exception("Lenghts do not match"))
    } else {
      if (!implicitly[List[String]].contains(candidateStr)){
        Left(new Exception("Word is not contained in the dictionary"))
      } else {
        Right(evalGuess(responseStr, candidateStr))
      }
    }

  def guess(wordc: String): State[WordleState, Word] = {
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
