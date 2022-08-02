package Wordle

case class TagState(tagged: Word,
                    bag: Map[Char, Int]
                   )

