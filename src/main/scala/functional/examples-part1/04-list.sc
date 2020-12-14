import functional.part1.intList._

val lst0 = IntCons(2, IntNil)
val lst1 = IntCons(1, lst0)
val lst2 = IntCons(3, lst0)

lst1.toString()

lst1.head
lst1.tail 