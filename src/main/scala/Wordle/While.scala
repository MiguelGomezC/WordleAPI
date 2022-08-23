package Wordle

import cats.data.State
import cats.data.State.{inspect, modify}

trait While{

  def whileState[A](evalB: A => Boolean)
                   (doSomething: A => A): State[A, Unit] = {
    modify(doSomething).flatMap(_ =>
    inspect(evalB(_)).flatMap(if (_) {whileState(evalB)(doSomething)}else{modify(identity)}))
  }
}
