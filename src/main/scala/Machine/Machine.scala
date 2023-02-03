package Machine

import Wordle.Word
import cats.Monad
import cats.data.StateT
import cats.implicits.toFunctorOps

import scala.language.existentials

trait Machine[F[_]]{
  type Q
  val current: Q
  val wordle: AbstractWordleButtons[F, Q]

}

object Machine{

  def apply[F[_], _Q](_current: _Q, _wordle: AbstractWordleButtons[F, _Q]): Machine[F] =
    new Machine[F]{
    type Q = _Q
    val current: _Q = _current
    val wordle: AbstractWordleButtons[F, _Q] = _wordle
  }

  def unapply[F[_]](m: Machine[F]): Option[(q, AbstractWordleButtons[F, q]) forSome { type q }] =
    Some((m.current, m.wordle))

  // Machine instructions

  class Instruction[F[_]: Monad] extends AbstractWordleButtons[F, Machine[F]]{

    def reset(): StateT[F, Machine[F], Unit] = StateT{ m: Machine[F] =>
      m.wordle.reset().run(m.current).map{
        case (s, b) => (Machine(s, m.wordle), b)
      }
    }

    def guess(i: String): StateT[F, Machine[F], Unit] = StateT{ m: Machine[F] =>
      m.wordle.guess(i).run(m.current).map{
        case (s, b) => (Machine(s, m.wordle), b)
      }
    }

    def check(): StateT[F, Machine[F], Word] = StateT{ m: Machine[F] =>
      m.wordle.check().run(m.current).map{
        case (s, b) => (Machine(s, m.wordle), b)
      }
    }

    def tries(): StateT[F, Machine[F], Int] = StateT{ m: Machine[F] =>
      m.wordle.tries().run(m.current).map{
        case (s, b) => (Machine(s, m.wordle), b)
      }
    }
  }
}