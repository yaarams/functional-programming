import functional.part3.functor.*
import functional.part3.graph.*

val lst1: List[Double] = List(1.0, 2.0, 5.0)
val lst2: List[String] = List("A", "B", "C")

// write it like this to make sure our map is called
Functor[List].map(lst1, _ * 2)
Functor[List].map(lst1, x => x + x)

val graph1 = Graph[String](
  nodes = List(
    "A", "B", "C"
  ),
  edges = List(
    Edge(0, 1),
    Edge(0, 2),
    Edge(2, 1)
  )
)


Functor[Graph].map(graph1, l => l.toLowerCase)
// based in extension we also have it as method
graph1.map(l => l + l + l)

val sqrtAll = Functor[List].lift(math.sqrt)

sqrtAll(lst1)
