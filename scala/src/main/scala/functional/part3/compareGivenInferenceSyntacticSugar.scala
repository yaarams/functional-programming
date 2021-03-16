package functional.part3

object compareGivenInferenceSyntacticSugar {


  // We use a strange name to not collide with anything in the standard lib
  trait Comp[A] {
    def compare(x: A, y: A): Int
  }


  // commonly used builder for summons to make syntax a little nicer   
  object Comp {
    inline def apply[A:Comp]: Comp[A] = summon[Comp[A]]
  }
  
  //val intOrdering = new Comp[Int] {
  given Comp[Int] {
    override def compare(x: Int, y: Int): Int = x - y
  }

  //val stringOrdering = new Comp[String] {
  given Comp[String] {
    override def compare(x: String, y: String): Int =
      // view will convert the string into a lazy sequence of chars, so if the first character
      // is different we wont evaluate all the other caracters in the string
      x.view.zip(y).map{case (xc, yc) => xc - yc}.find(_ != 0).getOrElse(0)
  }

  given[A: Comp, B: Comp] as Comp[(A, B)] {
    override def compare(x: (A, B), y: (A, B)): Int = {
      val ord1 = summon[Comp[A]].compare(x._1, y._1)
      if(ord1 == 0) {
        summon[Comp[B]].compare(x._2, y._2)
      } else {
        ord1
      }
    }
  }

  given[A: Comp, B: Comp, C: Comp] as Comp[(A, B, C)] {
    override def compare(x: (A, B, C), y: (A, B, C)): Int = {
      val ord1 = summon[Comp[A]].compare(x._1, y._1)
      if(ord1 == 0) {
        val ord2 = summon[Comp[B]].compare(x._2, y._2)
        if (ord2 == 0) {
          summon[Comp[C]].compare(x._3, y._3)
        } else {
          ord2
        }
      } else {
        ord1
      }
    }
  }
}
