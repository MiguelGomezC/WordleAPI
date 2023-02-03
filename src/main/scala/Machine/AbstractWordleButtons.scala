package Machine

import cats.{Applicative, FlatMap, Monad, MonadError}
import Wordle._
import cats.data.StateT
import cats.implicits.catsSyntaxMonadError

trait AbstractWordleButtons[F[_], Q] {
  def reset(): StateT[F, Q, Unit]
  def guess(word: String): StateT[F, Q, Unit]
  def check(): StateT[F, Q, Word]
  def tries(): StateT[F, Q, Int]
}

object AbstractWordleButtons {
  def apply[F[_] : Applicative, Q](_initial: Q,
                                   n: String => Q => Q,
                                   _check: Q => Word,
                                   _tries: Q => Int): AbstractWordleButtons[F, Q] = {
    /* Generic Apply */
    new AbstractWordleButtons[F, Q] {
      def reset(): StateT[F, Q, Unit] = StateT.set(_initial)

      def guess(word: String): StateT[F, Q, Unit] = StateT.modify(n(word))

      def check(): StateT[F, Q, Word] = StateT.inspect(_check)

      def tries(): StateT[F, Q, Int] = StateT.inspect(_tries)
    }
  }

  def apply[F[_], Q](_initial: Q,
                  _check: Q => Word,
                  _tries: Q => Int)
                 (validateLength: String => Boolean, validateFound: String => Boolean)
                 (n: String => Q => Q)
                 (implicit ae: MonadError[F, Error]): AbstractWordleButtons[F, Q] = {
    /* Apply depending on Error management function */
    new AbstractWordleButtons[F, Q] {
      def reset(): StateT[F, Q, Unit] = StateT.set(_initial)

      def guess(word: String): StateT[F, Q, Unit] =
        StateT
          .get
          .flatMap(n(word))
          .modify(n(word))


      def check(): StateT[F, Q, Word] = StateT.inspect(_check)

      def tries(): StateT[F, Q, Int] = StateT.inspect(_tries)
    }
  }
}