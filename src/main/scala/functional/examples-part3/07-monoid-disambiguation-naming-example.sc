import functional.part3.monoidDisambiguationByNaming.{productInt, sumInt, tupleMonoid, stringMonoid, mapMonoid, listMonoid, _}

(3 |+| 5)(sumInt) // explicitly pass the monoid definition for sum
(3 |+| 5)(productInt) // explicitly pass the monoid definition for product

"abc" |+| "def"


List(1, 2, 3) |+| List(4, 5, 6)

((1, "ab") |+| (3, "cd"))(tupleMonoid(sumInt, stringMonoid))
((1, "ab") |+| (3, "cd"))(tupleMonoid(productInt, stringMonoid))

val f = math.log |+| math.floor
f(30000)

// here we have 3 levels of monoid, compiler deriving it all
// Map -> Tuple -> (String, Int)
val m1 = Map(
  1 -> ("a", 4),
  2 -> ("c", 9)
)

val m2 = Map(
  1 -> ("b", 12),
  3 -> ("d", 7)
)

(m1 |+| m2)(mapMonoid(tupleMonoid(stringMonoid, sumInt)))
(m1 |+| m2)(mapMonoid(tupleMonoid(stringMonoid, productInt)))

val listOfLists = List(
  List(1, 4, 2),
  List(7),
  List(12, 12, 12, 12),
  List(4, 3)
)

val totalLength = listOfLists.map(_.length).combineAll(sumInt)
val totalProduct = listOfLists.map(_.length).combineAll(productInt)
