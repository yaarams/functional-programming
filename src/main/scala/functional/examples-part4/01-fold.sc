import functional.part1.pList._
import functional.part1.maybePart4._
import functional.part4.listFold._

def length[A](lst: PList[A]): Int =
  lst.foldLeft(0)((acc, _) => acc + 1)

def sum(lst: PList[Int]): Int =
  lst.foldLeft(0)((acc, x) => x + acc)

// this might not be the most efficient way to implement exists but it's general
def exists[A](lst: PList[A], v: A): Boolean =
  lst.foldLeft(false)((acc, x) => acc || x == v)

// we need to use foldLeft here because we construct linked lists from end to start
// if we don't care about the order we get, we can filter with foldLeft
def filter[A](lst: PList[A], p: A => Boolean): PList[A] =
  lst.foldRight(PList[A]())((x, acc) => if (p(x)) PCons(x, acc) else acc)

val lst = PList(1, 2, 3, 5, 6)

length(lst)
sum(lst)
exists(lst, 5)
exists(lst, 7)

def isOdd(x: Int): Boolean = x % 2 == 1

filter(lst, isOdd)
