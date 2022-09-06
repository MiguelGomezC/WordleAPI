package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.effect.IO
import cats.Monad

trait While{

  def repeatState[A](evalB: A => Boolean)
                   (doSomething: A => A): State[A, Unit] = {
    modify(doSomething).flatMap(_ =>
    inspect(evalB(_)).flatMap(if (_) {repeatState(evalB)(doSomething)}else{modify(identity)}))
  }

  //TODO: tail-recursive
  //@annotation.tailrec
  def whileIO[A](initial: A)(evalB: A => IO[Boolean])
                   (doSomething: A => IO[A]): IO[A] = {
    evalB(initial).flatMap(if (_){
      doSomething(initial).flatMap(whileIO(_)(evalB)(doSomething))} else {
      IO.pure(initial)
    })
  }

  /*
  def whileTailrec[A](initial: A)(evalB: A => IO[Boolean])
                (doSomething: A => IO[A]): IO[A] = {
    def go(initial:A)(evalB:A => IO[Boolean])(doSomething: A => IO[A])(acc: A): IO[A] ={
      evalB(initial).flatMap(if (_){
        doSomething(initial).flatMap(go(_)(evalB)(doSomething))} else {
        IO.pure(initial)
      })
    }

   */

}
