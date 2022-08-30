import Wordle._
import cats.effect.unsafe.implicits.global
import IOApp.run

object Main {
  def main(args: Array[String]): Unit = {
    //set dependencies
    val dictionary: List[String] = readResource("es.txt")
    //instantiate Wordle machine
    val wordleMachine: WordleAPI = WordleAPI(dictionary)
    //set UI and run game
    run(wordleMachine).unsafeRunSync()
  }
}
