package Wordle

import cats.Monad
import cats.data.{Reader, State}
import cats.data.State.{inspect, modify}

object WordleAPI {

  def evalGuess(responseStr: String, candidateStr: String): Word ={
    /* [G] coincidentes por posición
    * [Y] Dada una letra, min(conjunto(candidata), conjunto(oculta)) - coloreadas verdes */
    // READER. igual hasta lo puedo hacer con la mónada Id porque no hay dependencias que inyectar?
    val wordGreenColored: Word = responseStr.zip(candidateStr)
      .map(pair => if (pair._1 == pair._2) {
                      Letter(pair._1, Green)
                    } else Letter(pair._1, Black))
    val commonOcurrences: Map[Char, Int] = responseStr.toSet.map(charac =>
      (charac, responseStr.count(_==charac)
        .min(candidateStr.count(_==charac)))).toMap
    val numberOfGreenColored: Map[Char, Int] = responseStr.toSet.map(charac =>
      (charac, wordGreenColored.filter(_.color==Green).count(_.c==charac))).toMap
    val numberOfOccurencesToColorYellow: Map[Char, Int] = commonOcurrences
      .map{case (key,value) => (key, value - numberOfGreenColored.getOrElse(key, 0))}
    val wordYellowColored: Word = wordGreenColored.colorItYellow(numberOfOccurencesToColorYellow)

    println(wordYellowColored)
    wordYellowColored
  }

  def evalGuessReader: Reader[(String, String) ,Word] = {
    for {
      wordGreenColored <- Reader(case (responseStr: String, candidateStr: String) =>
      responseStr.zip(candidateStr).map(pair => if (pair._1 == pair._2) {
          Letter(pair._1, Green)
        } else Letter(pair._1, Black)))
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
