package Wordle

case class TagState(tagged: Word,
                    bag: Map[Char, Int]
                   )

case class BagWordState(hiddenWord: Word,
                        candidateWord: Word,
                        bag: Map[Char, Int])

