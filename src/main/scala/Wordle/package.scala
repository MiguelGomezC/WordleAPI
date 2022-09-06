import scala.io.Source
import scala.util.Using

package object Wordle extends Implicits {

  type Word = IndexedSeq[Letter]
  type Table = List[Word]

  /* Data driven computations */

  implicit class NextOp[A](a: A){

    def mapIf[B >: A](p: Boolean)(f: A => B): B =
      if (p) f(a)
      else a

    def mapAs[B](f: A => B): B =
      f(a)
  }

  /* Dictionaries */

  implicit class UpdatedMap[K, V](m: Map[K, V]){

    def modifyOrAdd(k: K)(n: V, f: V => V): Map[K, V] =
      if (!m.isDefinedAt(k)) m + (k -> n)
      else m.updated(k, f(m(k)))

    def modify(k: K)(f: V => V): Map[K, V] =
      if (!m.isDefinedAt(k)) m
      else m.updated(k, f(m(k)))

    def containsSuch(k: K)(p: V => Boolean): Boolean =
      m.get(k).exists(p)
  }

  /* Read-Write */

  def readResource(name: String): List[String] =
    Using(Source.fromResource(name)){ source =>
      source.getLines.toList
    }.getOrElse(List())

}
