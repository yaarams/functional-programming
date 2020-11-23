import functional.part1.intList._
import scala.collection.mutable.ArrayBuffer

val lst1 = IntCons(1, IntCons(2, (IntCons(3, IntNil))))

def toIntList(buffer: ArrayBuffer[Int]): IntList = {
  @annotation.tailrec
  def go(index: Int, acc: IntList): IntList = index match {
    case -1 => acc
    case i => go(i - 1, IntCons(buffer(i), acc))
  }

  go(buffer.length - 1, IntNil)
}

val list = toIntList(ArrayBuffer(1,2,3,4,5))

def map(f: (Int) => Int, lst: IntList): IntList = {
  // as long as this state is local and does not leak outside
  // of the function we still have a pure function
  val buffer = ArrayBuffer[Int]()

  // Unit equivelent to void (but isn't a special case as we will see later)
  @annotation.tailrec
  def feedBuffer(l: IntList): Unit = l match { 
    case IntNil => // Do nothing
    case IntCons(h, t) => 
      buffer.addOne(f(h))
      feedBuffer(t)
  }

  feedBuffer(lst)
  toIntList(buffer)
}

map(_ * 2, list)



