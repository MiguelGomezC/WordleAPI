import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should._
import Wordle._



class WordleSpec(tag: (String, String) => Word) extends AnyFlatSpec with Matchers{
  "matching characters" should "be detected" in {
    tag("word", "word") shouldBe IndexedSeq(Letter('w', Green), Letter('o', Green), Letter('r', Green), Letter('d', Green))
    tag("wor", "war").toVec should matchPattern { case Vector(('w', Green), _, ('r', Green)) => }
  }

  "non-matching characters" should "be detected" in {
    tag("vos", "war") shouldBe IndexedSeq(Letter('v', Black), Letter('o', Black), Letter('s', Black))
    tag("wor", "war").toVec should matchPattern { case Vector(_, ('o', Black), _) => }
    tag("rrrr", "warr").toVec should matchPattern { case Vector(('r', Black), ('r', Black), _, _) => }
  }

  "misplaced characters" should "be detected" in {
    tag("rwa", "war") shouldBe IndexedSeq(Letter('r', Yellow), Letter('w', Yellow), Letter('a', Yellow))
  }
}

object WordleSpec {
  def main(args:Array[String]): Unit = run(
    new WordleSpec(
      (candidate,response) => Next.evalGuess.evalGuess(response, candidate)
    )
  )
}