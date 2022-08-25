package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}
import cats.effect.IO

trait While{

  def repeatState[A](evalB: A => Boolean)
                   (doSomething: A => A): State[A, Unit] = {
    modify(doSomething).flatMap(_ =>
    inspect(evalB(_)).flatMap(if (_) {repeatState(evalB)(doSomething)}else{modify(identity)}))
  }

  def whileIO[A](initial: A)(evalB: A => IO[Boolean])
                   (doSomething: A => IO[A]): IO[Unit] = {
    evalB(initial).flatMap(if (_){
      doSomething(initial).flatMap(whileIO(_)(evalB)(doSomething))} else {
      IO.pure(initial)
    })
  }
}
