package uk.co.danielrendall.busybeaver

case class Tape(lowIndex: Int, highIndex: Int, ones: Set[Int]) {
  def read(index: Int): Int = if (ones.contains(index)) 1 else 0

  def write(index: Int, value: Int): Tape = value match {
    case 0 => Tape(newLowIndex(index), newHighIndex(index), ones - index)
    case 1 => Tape(newLowIndex(index), newHighIndex(index), ones + index)
    case _ => throw new IllegalArgumentException("Bad value: " + value)
  }

  @inline
  private def newLowIndex(index: Int) = Math.min(lowIndex, index)

  @inline
  private def newHighIndex(index: Int) = Math.max(highIndex, index)

  override def toString: String =
    Range.inclusive(lowIndex, highIndex).map(i => if (ones.contains(i)) '1' else '0').mkString(s"[$lowIndex] ", "", s" [$highIndex]")
}


object Tape {
  def init: Tape = Tape(0, 0, Set.empty)
}
