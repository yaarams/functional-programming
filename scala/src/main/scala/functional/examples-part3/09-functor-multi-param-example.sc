import functional.part3.functor.*


type Dictionary[A] = Map[String, A]

val m: Dictionary[Int] = Map(
  "a" -> 3,
  "b" -> 9,
  "c" -> 12
)


// m.map(x => x * 2) - We can't just do this because Map already has
// a map method which is defined as (K, V) => NEW_K_V and there is ambiguity
// If we don't directly specify that we're interested in the map of Functor
Functor[Dictionary].map(m, (x) => x * 2)
