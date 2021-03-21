import functional.part3.applicative.*

// an example we've seen befor but now with our abstraction
val m1 = Applicative[Option].pure(2)
val m2 = Applicative[Option].pure(7)
val m3 = Applicative[Option].pure(4)

def plus3(x: Int, y: Int, z: Int): Int = x + y + z

Applicative[Option].map3(m1, m2, m3, plus3)


// it's also a functor so we have map
Applicative[Option].map(m1, _ + 9)


// as methods extensions
val a:Option[Int] = Some(2)
a.ap(Applicative[Option].pure(_ + 12))