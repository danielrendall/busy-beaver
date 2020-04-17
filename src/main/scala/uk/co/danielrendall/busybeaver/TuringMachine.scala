package uk.co.danielrendall.busybeaver

/**
 * <Insert Description Here>
 *
 */
case class TuringMachine(tape: Tape, position: Int, state: Char, stateMap: Map[Char, State], ops: Int) {

  if (ops % 100000 == 0) {
    println(ops + ": " + position + "/" + state + " " + tape)
  }

  def run: Option[TuringMachine] =
    stateMap.get(state) match {
      case Some(value) => value match {
        case Active(_, onZero, onOne) =>
          val transition: Transition = tape.read(position) match {
            case 0 => onZero
            case 1 => onOne
            case _ => throw new IllegalStateException("Bad value on tape")
          }
          val newTape: Tape = tape.write(position, transition.valueToWrite)
          val newPosition: Int = transition.direction match {
            case Transition.Left => position - 1
            case Transition.Right => position + 1
          }
          val newState = transition.newStateIdentifier
          Some(TuringMachine(newTape, newPosition, newState, stateMap, ops + 1))
        case Halted =>
          println("HALTED after " + ops)
          println(tape)
          None
      }
      case None => throw new IllegalStateException(s"No state $state")
    }

}

object TuringMachine {

  def main(args: Array[String]): Unit = {
    val tm: Option[TuringMachine] = Some(TuringMachine())
    val list = LazyList.iterate(tm) {
      case Some(value) =>
        value.run
      case None =>
        None
    }
    list.find(_.isEmpty)
  }

  def apply(): TuringMachine = {
    // Add the halted state here
    val states: Seq[State] = getStates ++ Seq(Halted)
    val stateMap = states.map(s => s.identifier -> s).toMap
    TuringMachine(Tape(), 0, states.head.identifier, stateMap, 0)
  }

  def getStates: Seq[State] = getStates5

  /**
   * A 	  B
   * 0 	1RB 	1LA
   * 1 	1LB 	1RH
   *
   * @return
   */
  def getStates2: Seq[State] = Seq(
    State('A', "1RB", "1LB"),
    State('B', "1LA", "1RH")
  )

  /**
   * A 	  B 	  C
   * 0 	1RB 	0RC 	1LC
   * 1 	1RH 	1RB 	1LA
   *
   * @return
   */
  def getStates3: Seq[State] = Seq(
    State('A', "1RB", "1RH"),
    State('B', "0RC", "1RB"),
    State('C', "1LC", "1LA")
  )

  /**
   * A   	B 	  C 	  D
   * 0 	1RB 	1LA 	1RH 	1RD
   * 1 	1LB 	0LC 	1LD 	0RA
   *
   * @return
   */
  def getStates4: Seq[State] = Seq(
    State('A', "1RB", "1LB"),
    State('B', "1LA", "0LC"),
    State('C', "1RH", "1LD"),
    State('D', "1RD", "0RA")
  )

  /**
   * A 	  B 	  C 	  D 	  E
   * 0 	1RB 	1RC 	1RD 	1LA 	1RH
   * 1 	1LC 	1RB 	0LE 	1LD 	0LA
   *
   * @return
   */
  def getStates5: Seq[State] = Seq(
    State('A', "1RB", "1LC"),
    State('B', "1RC", "1RB"),
    State('C', "1RD", "0LE"),
    State('D', "1LA", "1LD"),
    State('E', "1RH", "0LA")
  )
}

