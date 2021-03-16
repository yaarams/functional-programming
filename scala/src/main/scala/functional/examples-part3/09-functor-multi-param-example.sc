import functional.part3.functor._

val m = Map(
  "a" -> 3,
  "b" -> 9,
  "c" -> 12
)

// m.map(x => x * 2) - if we don't specify type hints, compiler can't
// understand if we want to use the new map we deined or the map that exists
// on map which is (K,V) => (K, V)
m.map((x: Int) => x * 2)

