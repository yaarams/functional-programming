import functional.part3.compareGivenInference.*


// def bubbleSort[A](lst: List[A], cmp: Comp[A]): List[A] = {
def bubbleSort[A](lst: List[A])(using cmp: Comp[A]): List[A] = {
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

bubbleSort(lst1)
bubbleSort(lst2)


val lst3 = List(("ab", 7), ("aa", 4), ("aa", 3))
val lst4 = List((1, 2), (3, 4), (1, 1))

bubbleSort(lst3)
bubbleSort(lst4)

