package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}

class WordleAPI {
  val defaultHiddenWord = "LUMEN"
  val defaultDict = Map("LUMEN" -> " Unidad de flujo luminoso del sistema internacional")

  def evalGuess(hiddenWord: String, guess: String): Word ={
    /* [G] coincidentes por posición
    * [Y] Dada una letra, min(conjunto(candidata), conjunto(oculta)) - coloreadas verdes
    * */
    val wordGreenColored: Word = hiddenWord.zip(guess)
      .map(pair => if (pair._1 == pair._2) {
        Letter(pair._1, Green)
      } else Letter(pair._1, Black))
    /* coloreación en amarillo: enseñar número de letras
     que coinciden de una y otra palabra MENOS ocurrencias verdes.
      -calcular coincidencias y hacer el mínimo: set(hiddenWord)
      -restar el número de verdes: filter(verde) de hiddenword y restar mapas
      -colorear el número de letras */

    /* proceso más lineal: bolsa + verdes en una primera pasada
    * transformador de estados 1: bolsa + 2: consumición de la misma */
    val commonOcurrences: Map[Char, Int] = hiddenWord.toSet.map(letra =>
      (letra, hiddenWord.count(_==letra)
        .min(guess.count(_==letra)))).toMap
    val numberOfGreenColored: Map[Char, Int] = hiddenWord.toSet.map(letra =>
      (letra, wordGreenColored.filter(_.color==Green).count(_.c==letra))).toMap
    val numberOfOccurencesToColorYellow: Map[Char, Int] = commonOcurrences
      .map{case (key,value) => (key, value - numberOfGreenColored.getOrElse(key, 0))}

    val wordYellowColored: Word = wordGreenColored
    /* meter a lo mejor un println aquí */
    println(wordYellowColored)
    wordYellowColored
  }

  def guess(wordc: String): State[WordleState, Word] =
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
