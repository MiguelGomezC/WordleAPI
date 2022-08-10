package Wordle

case class BagWordState(hiddenWord: Word,
                        candidateWord: Word,
                        bag: Map[Char, Int])
