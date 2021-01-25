package functional.part4

import functional.part1.maybePart4._
import functional.part1.pList._
import functional.part4.listFold._

object maybeSequence {
  
  // pattern matching based implementation
  def sequenceList[A](lm: PList[Maybe[A]]): Maybe[PList[A]] = lm match {
    case PNil => Maybe.pure(PNil)
    case PCons(head, tail) => Maybe.map2(head, sequence(tail), (h, t) => PCons(h, t))
  }
  
  // more general implementation using fold 
  // This will be interesting in the future when we talk about abstractions
  // because it seems we can implement sequence for every type (Maybe here) that has pure and map2
  // If we can somehow abstract over Maybe here, this functions might be a lot more general
  def sequence[A](lm: PList[Maybe[A]]): Maybe[PList[A]] = {
    val zero = Maybe.pure[PList[A]](PNil)
    lm.foldRight(zero)((head, acc) => Maybe.map2(head, acc, (h, t) => PCons(h, t)))
  }
  
}
