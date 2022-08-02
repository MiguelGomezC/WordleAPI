import cats.data.State
import cats.implicits._

package object Wordle extends Implicits {

  type Word = IndexedSeq[Letter]
  type Table = IndexedSeq[Word]

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

  implicit class YellowColoredWord(wordc: Word) {
    def colorItYellow(occurrencesToColor: Map[Char, Int]): Word = {
      wordc.toList.traverse {
        (letter: Letter) => State.modify[TagState]{
          case TagState(tagged: Word, bag: Map[Char, Int]) =>
            val isGreen = !letter.color.equals(Green)
            TagState(
              tagged
                .mapAs(_ :+ letter)
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ :+ letter.copy(color = Yellow)),
              bag
                .mapIf(isGreen & bag.containsSuch(letter.c)(_>0))(_ => bag.modify(letter.c)(_-1))
            )
        }
      }.inspect(_.tagged).runA(TagState("".toBlackWord, occurrencesToColor)).value
    }
  }
}
