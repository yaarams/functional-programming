package functional.part3

import functional.part3.functor._
import scala.collection.mutable.ArrayBuffer

object applicative {

  trait Applicative[F[_]] extends Functor[F] {
    
    def pure[A](a: A): F[A]

    // Implement ap and map2 in terms of each other, giving the implementer the option to implement one of them 
    // which ever he find more convinient
    def ap[A, B](fa: F[A], ff: F[A => B]): F[B] =
      map2(fa, ff, (a, f) => f(a))

    
    def map2[A, B, C](a: F[A], b: F[B], f: (A, B) => C): F[C] = 
      ap(b, map(a, f.curried))
    
    // --------------------- //
    // Implement the Functor //
    // --------------------- //
    
    override def map[A, B](fa: F[A], f: A => B): F[B] = ap(fa, pure(f))

    // ---------------- //
    // Derived Function //
    // ---------------- //

    def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = (fa, fb) => map2(fa, fb, f)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C], f: (A, B, C) => D): F[D] = ap(fc, ap(fb, map(fa, f.curried)))

    def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] = (fa, fb, fc) => map3(fa, fb, fc, f)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb, (_, _))
    
    def product[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = map3(fa, fb, fc, (_, _, _))
  
    // In the future we will see another abstraction making this method work with other things, not just List  
    def sequence[A](lst: List[F[A]]): F[List[A]] = lst match {
      case Nil => pure(Nil)
      case fh :: ft => ap(sequence(ft), map(fh, h => h :: _))
    }
    
  }

  object Applicative {
    // a sligntly nicer syntax for summon
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

  // ----------------------------- //
  // Applicative Functor instances //
  // ----------------------------- //

  given Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    // Implement ap and not map2
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

    // Implement map2 instead of ap
    override def map2[A, B, C](fa: Option[A], fb: Option[B], f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  }

  // Map is not an applicative functor because we can't implement pure for it  

}
