package uk.co.danielrendall.busybeaver

sealed trait State {
  def identifier: Char
}

case class Active(identifier: Char, onZero: Transition, onOne: Transition) extends State

case object Halted extends State {
  override val identifier: Char = 'H'
}

object State {
  def apply(identifier: Char, onZero: String, onOne: String): Active =
    Active(identifier, Transition(onZero), Transition(onOne))
}
