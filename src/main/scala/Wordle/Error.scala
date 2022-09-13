package Wordle

sealed trait Error
case object LengthMismatch extends Error
case object WordNotFound extends Error
