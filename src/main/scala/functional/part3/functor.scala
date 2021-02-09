package functional.part3

object functor {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A], f: A => B): F[B]

    // ----------------- //
    // Derived Functions //
    // ----------------- //

    def lift[A, B](f: A => B): F[A] => F[B] = (m) => map(m, f)

  }

  object Functor {
    // a sligntly nicer syntax for summon
    def apply[F[_]: Functor]: Functor[F] = summon[Functor[F]]
  }

  extension[F[_]: Functor, A, B](fa: F[A]) {
    def map(f: A => B): F[B] = Functor[F].map(fa, f)
  }

  // ----------------- //
  // Functor instances //
  // ----------------- //

  given Functor[List] {
    override def map[A, B](fa: List[A], f: A => B): List[B] = fa.map(f)
  }

  given Functor[Array] {
    override def map[A, B](fa: Array[A], f: A => B): Array[B] = fa.map(f)
  }

  given Functor[Option] {
    override def map[A, B](fa: Option[A], f: A => B): Option[B] = fa.map(f)
  }
  
  // --------------------------------------------------------- //
  // Functor instances for types with multiple type parameters //
  // --------------------------------------------------------- //

  // [X] => Map[K, X] is a type lambda, sort of like a function on the type level
  // It says that if you proved type X it will produce a type
  // Another way to say it is that Map here is partially applied with K 
  // (similar to partailly applied functions but with types)
  given[K] as Functor[[X] =>> Map[K, X]] {
    override def map[A, B](fa: Map[K , A], f: A => B): Map[K, B] = fa.map((k, v) => (k, f(v)))
  }
  
  // ------------------- //
  // Functor Composition //
  // ------------------- //
  
  // This can be written directly on trait Functor, we use extension here just to show this gradually
  extension[F[_], G[_]](FF: Functor[F]) {
    def compose(GF: Functor[G]): Functor[[X] =>> F[G[X]]] =  new Functor {
      override def map[A, B](fga: F[G[A]], f: A => B): F[G[B]] = {
        FF.map(fga, ga => GF.map(ga, f))
      }
    }
  }

}
