package uk.co.danielrendall.busybeaver

import uk.co.danielrendall.busybeaver.Transition.Direction

case class Transition(newStateIdentifier: Char, valueToWrite: Int, direction: Direction)

object Transition {
  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction

  def apply(string: String): Transition = {
    assert(string.length == 3, s"Bad string: '$string'")
    val valueToWrite: Int = if (string.charAt(0) == '1') 1 else 0
    val direction: Direction = if (string.charAt(1) == 'L') Left else Right
    val nextState: Char = string.charAt(2)
    Transition(nextState, valueToWrite, direction)
  }
}
