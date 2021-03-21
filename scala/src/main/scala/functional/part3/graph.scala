package functional.part3

import functional.part3.functor.Functor

object graph {

  type NodeIndex = Int

  case class Edge(from: NodeIndex, to: NodeIndex)

  case class Graph[A](
      nodes: List[A],
      edges: List[Edge]               
  )
  
  given Functor[Graph] with {
    def map[A, B](fa: Graph[A], f: A => B): Graph[B] = fa.copy(nodes=fa.nodes.map(f))
  }  
  
}
