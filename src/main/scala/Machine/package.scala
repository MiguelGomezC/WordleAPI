import cats.data.StateT

package object Machine {
  type Program[F[_], A] = StateT[F, Machine[F], A]
}