package Wordle

case class TagState(tagged: Word,
                    bag: Map[Letter, Int],
                    index: Int)

