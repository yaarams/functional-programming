// scala builtin Either type
import functional.part2.sentOp._

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
def getChildIndex(fromIndex: TokenIndex, label: String): SentOp[TokenIndex] = SentOp(
  sent => sent.edges
    .filter(edge => edge.from == fromIndex && edge.label == label)
    .map(edge => edge.to)
)

val ateIndex = 3
val jumpedIndex = 11

val dobj = getChildIndex(_, "dobj")
val advmod = getChildIndex( _, "advmod")
val det = getChildIndex( _, "det")

val detsFromAte = for (
  dobjIndex <- dobj(ateIndex);
  detIndex <- det(dobjIndex)
) yield sentence.tokens(detIndex)

detsFromAte.run(sentence)

val detsFromJumped = for(
  dobjIndex <- dobj(jumpedIndex);
  detIndex <- det(dobjIndex)
) yield sentence.tokens(detIndex)

detsFromJumped.run(sentence)


def opEdge(index: TokenIndex): SentOp[Edge] =
  for (
    sentence <- SentOp.sentenceInContext();
    verb = sentence.tokens(index);
    from <- advmod(index);
    to <- dobj(index)
  ) yield Edge(from, to, verb)

opEdge(ateIndex).run(sentence)

// get all opEdges in the sentence

val allOpEdges = for (
  sentence <- SentOp.sentenceInContext();
  i <- SentOp.pureMany(sentence.tokens.indices.toList);
  edge <- opEdge(i)
) yield edge


allOpEdges.run(sentence)

val sentence2 = Sentence(
  tokens = List("Jim", "walked", "the", "dog"),
  edges = List(
    Edge(1, 0, "advmod"),
    Edge(1, 3, "dobj"),
    Edge(3, 2, "det"),
  )
)

allOpEdges.run(sentence2)

