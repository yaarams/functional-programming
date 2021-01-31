import functional.part3.applicative._

val lst1: List[Option[Int]] = List(Some(1), Some(2), Some(5))

lst1.sequence

val lst2: List[Option[Int]] = List(Some(1), None, Some(5))

lst2.sequence
