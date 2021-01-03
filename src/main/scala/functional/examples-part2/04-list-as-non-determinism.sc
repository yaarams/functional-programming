// scala builtin Either type
import functional.part2.list._

type TokenIndex = Int
case class Edge(from: TokenIndex, to: TokenIndex, label: String)
case class Sentence(tokens: List[String], edges: List[Edge])


val sentence = Sentence(
  tokens = List("Marry", "and", "Sally", "ate", "an", "organe", "and", "a", "bannana", "while", "Betty", "jumped", "the", "rope"),
  edges = List(
    Edge(3, 0, "advmod"),
    Edge(3, 2, "advmod"),
    Edge(11, 10, "advmod"),
    Edge(3, 5, "dobj"),
    Edge(3, 5, "dobj"),
    Edge(11, 13, "dobj"),
    Edge(5, 4, "det"),
    Edge(8, 7, "det"),
    Edge(13, 12, "det"),
  )
)

// This can be viewed as non deterministic computation where it can result in 0..n results
def getChildIndex(sent: Sentence, fromIndex: TokenIndex, label: String): List[TokenIndex] =
  sent.edges
    .filter(edge => edge.from == fromIndex && edge.label == label)
    .map(edge => edge.to)

// partial functions, we fixate it to the one sentence example, but we will later see how we can keep that as a parameter
val dobj = getChildIndex(sentence, _, "dobj")
val advmod = getChildIndex(sentence, _, "advmod")
val det = getChildIndex(sentence, _, "det")

val ateIndex = 3
val jumpedIndex = 11

for (
  dobjIndex <- dobj(ateIndex);
  detIndex <- det(dobjIndex)
) yield sentence.tokens(detIndex)

for(
  dobjIndex <- dobj(jumpedIndex);
  detIndex <- det(dobjIndex)
) yield sentence.tokens(detIndex)


def opEdge(index: TokenIndex): List[Edge] = {
  val verb = sentence.tokens(index)
  map2(advmod(index), dobj(index), Edge(_, _, verb))
}

opEdge(ateIndex)

// get all opEdges in the sentence
sentence.tokens.indices.flatMap(opEdge)

for (
  i <- sentence.tokens.indices;
  edge <- opEdge(i)
) yield edge
