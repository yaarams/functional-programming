import functional.part3.applicative._

val lst1: List[Option[Int]] = List(Some(1), Some(2), Some(5))

lst1.sequence

val lst2: List[Option[Int]] = List(Some(1), None, Some(5))

lst2.sequence

val lst3: List[(Int, Int)] = List((1, 2), (3, 4), (5, 6))
// lst3.sequence -> compiler doesn't know what to do with it
// for some reason so we manualy tell it what applicative we have here
Applicative[[X] =>> (X, X)].sequence(lst3)


// here the compiler isn't confused so with partaially applied types it's always
// woth working with type aliases to help the compiler (maybe it wont be an issue with
// future versions of scala but it's better to be safe)
type Pair[A] = (A, A)
val lst4: List[Pair[Int]] = lst3
lst4.sequence

val opt1: List[Option[Pair[Int]]] = List(Some((1, 2)), Some((3, 7)), Some((4, 2)))

// only swapper List with Option
opt1.sequence

// but we can explicitly say that Option[Pair] is an applictative and push
// List two levels in
(Applicative[Option] compose Applicative[Pair]).sequence(opt1)


