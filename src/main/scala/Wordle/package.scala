import Wordle.TagState._
import cats.data.State
import cats.implicits._


package object Wordle {

  type Word = IndexedSeq[Letter]
  type Table = IndexedSeq[Word]

  implicit class WordFromString(str: String) {
    def toBlackWord: Word = str.map((char: Char) => Letter(char, Black))
  }

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
    def colorItYellow(occurrencesToColor: Map[Letter, Int]): Word = {
      wordc.toList.traverse {
        (letter: Letter) => State.modify[TagState]{
          case TagState(tagged: Word, bag: Map[Letter, Int], index: Int) =>
            val isGreen = !letter.color.equals(Green)
            /* Control driven queda más simple aquí */
            TagState(
              tagged
                .mapAs(_ :+ letter)
                .mapIf(isGreen & bag.containsSuch(letter)(_>0))(_ :+ letter.copy(color = Yellow)),
              bag
                .mapIf(isGreen & bag.containsSuch(letter)(_>0))(_ => bag.modify(letter)(_-1)),
              index
            )
        }
      }.inspect(_.tagged).runA(TagState("".toBlackWord, occurrencesToColor, 0)).value
    }
  }
}
