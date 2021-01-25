import functional.part3.compare._

def bubbleSort[A](lst: List[A], cmp: Comp[A]): List[A] = {
  // lets use local mutation
  val sortedBuffer = lst.toBuffer

  for (i <- 1 until sortedBuffer.length) {
    for (j <- 0 until i) {
      if (cmp.compare(sortedBuffer(i), sortedBuffer(j)) < 0) {
        val tmp = sortedBuffer(j)
        sortedBuffer(j) = sortedBuffer(i)
        sortedBuffer(i) = tmp
      }
    }
  }

  sortedBuffer.toList
}

val lst1 = List(3, 1, 7, 3, 87, 2)
val lst2 = List("ba", "ab", "aa", "bb", "c")

bubbleSort(lst1, intOrdering)
bubbleSort(lst2, stringOrdering)
