import functional.part3.functor._

val lst1: List[Option[Int]] = List(Some(1), Some(2), Some(5))

(Functor[List] compose Functor[Option]).map(lst1, _ * 2)

val map1: Map[String, Option[Int]] = Map("a" -> Some(1), "b" -> Some(2), "c" -> Some(5))

// getting an instance can be diffeculy it we need to express the type explicitly for composition
// Type lambdas are ok when implementing a lib, not so easy to read in regular code
(Functor[[X] =>> Map[String, X]] compose Functor[Option]).map(map1, _ * 2)

// If we don't want to deal with type lambdas at usage
// sometimes type aliases are more readable
type Dictionary[A] = Map[String, A]

(Functor[Dictionary] compose Functor[Option]).map(map1, _ + 12)

