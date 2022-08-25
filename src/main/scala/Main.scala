import Wordle._
import cats.effect.unsafe.implicits.global
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //set dependencies
    val dictionary: List[String] = readResource("es.txt")
    val random = new Random(seed = 1000)
    val hiddenStr: String = dictionary(
      random.nextInt(dictionary.length)
    )
    //instantiate Wordle machine
    val wordleMachine: WordleAPI = WordleAPI(hiddenStr, dictionary)
    //set UI and run game
    Wordle.run(wordleMachine).unsafeRunSync()
  }
}
