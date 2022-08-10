import Wordle._

object Main {
  def main(args: Array[String]): Unit = {
    val hiddenStr: String = implicitly[String]
    WordleAPI.run(hiddenStr)
  }
}
