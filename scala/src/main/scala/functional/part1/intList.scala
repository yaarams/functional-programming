package functional.part1

object intList {

  sealed trait IntList {

    override def toString(): String = this match {
      case IntCons(h, t) => f"${h}:${t.toString}"
      case IntNil => "Nil"
    }
  }

  case class IntCons(head: Int, tail: IntList) extends IntList

  // notice Nil is an object, we don't need a whole class, it's always a single value
  object IntNil extends IntList;
  
}