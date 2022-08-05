package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.effect.IO

import  Wordle.evalGuess._

object WordleAPI {

  def attemptComparison(responseStr: String, candidateStr: String)
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

  def attempt(responseStr: String, candidateStr: String): Word =
    attemptComparison(responseStr, candidateStr) match {
      case Left(msg) => println(msg); attempt(responseStr, candidateStr)
      case Right(word) => word
    }

  def runGame(wordc: String): State[WordleState, Word] = {
    /* usar algo como iterate aquí */
    for {
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
      word <- inspect((tablero: WordleState) => attempt(tablero.hiddenWord, wordc))
      _ <- modify((tablero: WordleState) => tablero.copy(table = tablero.table :+ word))
    } yield word
  }

}
