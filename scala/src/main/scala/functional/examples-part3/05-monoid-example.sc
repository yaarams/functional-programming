import functional.part3.monoid.*

3 |+| 5

"abc" |+| "def"

List(1, 2, 3) |+| List(4, 5, 6)

(1, "ab") |+| (3, "cd")

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

m1 |+| m2


val listOfLists = List(
  List(1, 4, 2),
  List(7),
  List(12, 12, 12, 12),
  List(4, 3)
)

val totalLength = listOfLists.map(_.length).combineAll

// probably less efficient but also valid
val totalLength2 = listOfLists.combineAll.length
