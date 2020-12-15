// scala builtin Either type
import functional.part2.listZippers._

val lst1 = List(1, 2, 3, 4, 5, 6)

lst1.runZipper(insertAfterSecond(99))

