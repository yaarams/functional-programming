import functional.part1.pList._
import functional.part1.maybePart4._
import functional.part3.maybeSequence._


val lst1 = PList(Maybe.pure(1), Maybe.pure(2), Maybe.pure(3))
val lst2 = PList(Maybe.pure(1), Maybe.nothing(), Maybe.pure(3))


sequence(lst1)
sequence(lst2)
