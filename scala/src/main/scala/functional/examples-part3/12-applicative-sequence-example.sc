import functional.part3.applicative.*

val lst1: List[Option[Int]] = List(Some(1), Some(2), Some(5))

val result1: Option[List[Int]] = lst1.sequence

val lst2: List[Option[Int]] = List(Some(1), None, Some(5))

val result2: Option[List[Int]] = lst2.sequence
