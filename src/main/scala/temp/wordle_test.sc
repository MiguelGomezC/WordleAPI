import cats.data._
import cats.syntax.all._
import cats.instances.all._
import State.{get, inspect, modify, set}
import cats.syntax.traverse

import scala.:+
import scala.io.StdIn.readLine

/* ESTRUCTURAS DE DATOS BASE */
case class Letter(c: Char, color: String)

type Word = IndexedSeq[Letter]

type Table = IndexedSeq[Word]

case class WordleState(table: Table, dict: Map[String, String], hiddenWord: String)

object WordleState {
  import WordCreatorHelper._

  def apply(dict: Map[String, String], hiddenWord: String): WordleState = {
    /* Set initial state */
    WordleState(
      table = IndexedSeq(hiddenWord.toBlackWord),
      dict = dict,
      hiddenWord = hiddenWord
    )
  }
}

/* UTILS */
object WordCreatorHelper {
  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map(char => Letter(char, "B"))
  }
  implicit class YellowColoredWord(word: Word) {
    def colorItYellow(occurencesToColor: Map[Char, Int]): Word = {
      /* colorear en amarillo evitando las que había en verde */
      ???
    }
  }
}

/* API */
class WordleAPI {

  val hiddenWord = "LUMEN"
  val defaultDict = Map("LUMEN" -> " Unidad de flujo luminoso del sistema internacional")

  /* Mejorar la Robustez/detección de errores: y si las palabras no miden lo mismo? */
  def evalGuess(hiddenWord: String, guess: String): Word ={
    /* coloreación en verde: letras coincidentes por posición */
    val wordGreenColored: Word = hiddenWord.zip(guess)
      .map(pair => if (pair._1 == pair._2) {
      Letter(pair._1, "G")
    } else Letter(pair._1, "B"))
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
      (letra, wordGreenColored.filter(_.color=="G").count(_.c==letra))).toMap
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
    } yield word
}

/*
ESPECIFICACIÓN

Quiero que se declare la palabra a adivinar (no tiene por qué meterse por consola)
Quiero mostrar por pantalla la tabla antes de cada guess


Quiero almacenar la tabla entera en el estado, además del diccionario y la palabra a adivinar
Quiero un input(string) dentro de guess que pida la palabra candidata
Diccionario: estructura que puede estar en local
 */

def newGame(dict: Map[String, String], hiddenWord: String): State[WordleState, Word] = {
  import WordleAPI._
  /* La intención es hacer guess(readline()) */
  for {
    _ <- set(WordleState(dict, hiddenWord))
    s <- WordleAPI.guess(readLine)
  } yield ()
}
