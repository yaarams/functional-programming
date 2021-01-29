package functional.part3

import functional.part3.functor._
import scala.collection.mutable.ArrayBuffer

object applicative {

  trait Applicative[F[_]] {
    
    def pure[A](a: A): F[A]

    def ap[A, B](fa: F[A], ff: F[A => B]): F[B]

    // --------------------- //
    // Implement the Functor //
    // --------------------- //
    
    def map[A, B](fa: F[A], f: A => B): F[B] = ap(fa, pure(f))

    // ---------------- //
    // Derived Function //
    // ---------------- //
    
    def map2[A, B, C](a: F[A], b: F[B], f: (A, B) => C): F[C] = ap(b, map(a, f.curried))

    def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = (a, b) => map2(a, b, f)

    def map3[A, B, C, D](a: F[A], b: F[B], c: F[C], f: (A, B, C) => D): F[D] = ap(c, ap(b, map(a, f.curried)))

    def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] = (a, b, c) => map3(a, b, c, f)

    def product[A, B](a: F[A], b: F[B]): F[(A, B)] = map2(a, b, (_, _))
  
    // In the future we will see another abstraction making this method work with other thengs, not just List  
    def sequence[A](lst: List[F[A]]): F[List[A]] = lst match {
      case Nil => pure(Nil)
      case fh :: ft => ap(sequence(ft), map(fh, h => h :: _))
    }
    
  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = summon[Applicative[F]]

    def compose[F[_], G[_]](FF: Applicative[F], GF: Applicative[G]): Applicative[[X] =>> F[G[X]]] = new Applicative {

      override def pure[A](a: A): F[G[A]] = FF.pure(GF.pure(a))

      override def ap[A, B](fga: F[G[A]], ff: F[G[A => B]]): F[G[B]] = {
        val p: F[(G[A], G[A=>B])] = FF.product(fga, ff)
        FF.map(p, GF.ap(_, _))
      }

    }
    
  }

  extension[F[_]: Applicative, A, B](fa: F[A]) {
    def ap(f: F[A => B]): F[B] = Applicative[F].ap(fa, f)
  }

  extension[F[_]: Applicative, A](lst: List[F[A]]) {
    def sequence: F[List[A]] = Applicative[F].sequence(lst)
  }

  extension[F[_], G[_]](FF: Applicative[F]) {
    def compose(GF: Applicative[G]): Applicative[[X] =>> F[G[X]]] = Applicative.compose(FF, GF)
  }

  // This defintion should be in functor implementation but we placed it here because of the order of teaching
  given[F[_]: Applicative] as Functor[F]  {
    override def map[A, B](fa: F[A], f: A => B): F[B] = Applicative[F].map(fa, f)
  }
  
  // ----------------------------- //
  // Applicative Functor instances //
  // ----------------------------- //

  given Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](fa: List[A], ff: List[A => B]): List[B] = {
      // local mutation
      val buffer = ArrayBuffer[B]()

      fa.foreach(a => {
        buffer.addAll(ff.map(f => f(a)))
      })

      buffer.toList 
    }
  }

  given Applicative[Option] {
    
    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](fa: Option[A], ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (Some(a), Some(f)) => Some(f(a))
      case _ => None
    }
  }

  // Map is not an applicative functor because we can't implement pure for it  

  
  given Applicative[[X] =>> (X, X)] {
    override def pure[A](a: A): (A, A) = (a, a)

    override def ap[A, B](fa: (A, A), ff: (A => B, A => B)): (B, B) = (ff._1(fa._1), ff._2(fa._2))
  }

  given Applicative[[X] =>> (X, X, X)] {
    override def pure[A](a: A): (A, A, A) = (a, a, a)

    override def ap[A, B](fa: (A, A, A), ff: (A => B, A => B, A => B)): (B, B, B) = (ff._1(fa._1), ff._2(fa._2), ff._3(fa._3))
  }

}
