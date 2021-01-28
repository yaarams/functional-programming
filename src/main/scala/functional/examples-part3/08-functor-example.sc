import functional.part3.functor._

val lst1: List[Option[Int]] = List(Some(1), Some(2), Some(5))

(Functor[List] compose Functor[Option]).map(lst1, _ * 2)

val map1: Map[String, Option[Int]] = Map("a" -> Some(1), "b" -> Some(2), "c" -> Some(5))

// getting an instance can be diffeculy it we need to express the type explicitly for composition
(Functor[[X] =>> Map[String, X]] compose Functor[Option]).map(map1, _ * 2)

// If we don't want to deal with type lambdas at usage
// sometimes type aliases are more readable
type Dictionary[A] = Map[String, A]

(Functor[Dictionary] compose Functor[Option]).map(map1, _ + 12)

type Pair[A] = (A, A)

// F can map over 3 levels, behaving correctly on every level
val F = Functor[List] compose Functor[Option] compose Functor[Pair]

val opt1: List[Option[Pair[Int]]] = List(Some((1, 2)), Some((3, 7)), Some((4, 2)))
F.map(opt1, _ * 2)


val opt2: List[Option[Pair[Int]]] = List(Some((1, 2)), None, Some((4, 2)))
F.map(opt2, _ * 2)


