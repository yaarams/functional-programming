import functional.part3.applicative.*

val F = Applicative[List] compose Applicative[Option]

F.pure(7)

val lst1: List[Option[Double]] = List(Some(4.0), Some(9.0), Some(16.0))
F.map(lst1, _ + 5) // applicative is also a functor so we get map


val lst2: List[Option[Double]] = List(Some(0.5), Some(1.0))

F.map2(lst1, lst2, _ * _)


val lst3: List[Option[Double]] = List(Some(4.0), None, Some(16.0))


F.map2(lst3, lst2, _ * _)

F.product(lst3, lst2)


val lst4: List[List[Option[Double]]] = List(
  List(Some(-4.0), None, Some(-16.0)),
  List(Some(4.0), Some(9.0), Some(16.0))
)


lst4.sequence // Swaps the two lists, effectively doing nothing

F.sequence(lst4) // pushes the outer list two levels in
