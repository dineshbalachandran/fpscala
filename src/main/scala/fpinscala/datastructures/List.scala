package fpinscala.datastructures

import fpinscala.datastructures.List.x

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case _ => 101
    case Cons(h, t) => h + sum(t)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => Nil
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def loop(s: List[A], acc: List[A]) : List[A] = s match {
      case Cons(_, Nil) => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }
    reverse(loop(l, Nil))
  }

  def reverse[A](l: List[A]): List[A] = {
    def loop(s: List[A], acc: List[A]) : List[A] = s match {
      case Nil => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }
    loop(l, Nil)
  }

  def foldReverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((a,b) => Cons(b,a))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x) => {x+1})
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => {x+1})

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(s: List[A], acc:B) : B = s match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(l, z)
  }

  def foldRightLeft [A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    def g(b: B, a: A) = f(a, b)
    foldLeft(as, z)(g)
  }

  def foldAppend[A](x: List[A], y:List[A]): List[A] = foldRight(x,y)(Cons(_,_))

  def flatList[A](l: List[List[A]]) : List[A] = {
    foldRight(l, Nil:List[A])(append(_,_))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a,b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a,b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, b) => append(f(a),b))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C) : List[C] = {
    def loop(l:(List[A], List[B])): List[C] = l match {
      case (Nil, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), loop((at, bt)))
    }
    loop((a,b))
  }

  def hasSubsequence[A](sup: List[A], sub:List[A]): Boolean = {
    @annotation.tailrec
    def loop(a: (List[A], List[A])): Boolean = a match {
      case (_, Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(ah, at), Cons(bh, bt)) => { if (ah == bh) loop((at, bt)) else loop((at, sub)) }
    }
    loop((sup, sub))
  }
}

object ListTest {
  def main(args: Array[String]): Unit = {
    println(List.x)
    println(List.tail(List(1,2,3)))
    println(List.setHead(List(1,2,3), 4))
    println(List.dropWhile(List(1,2,3), (x:Int) => {x < 3}))
    println(List.dropWhile(Nil, (x:Int) => {x < 2}))
    println(List.init(List(1,2,3)))
    println(List.length(List(1,2,3)))
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(List.foldLeft(List(1,2,3), Nil:List[Int])((a,b) => Cons(b,a)))
    println(List.length2(List(1,2,3)))
    println(List.foldReverse(List(1,2,3)))
    println(List.foldRightLeft(List(1,2,3), Nil:List[Int])(Cons(_,_))) //wrong
    println(List.foldAppend(List(1,2,3), List(4,5,6)))
    println(List.flatList(List(List(1,2,3,4), List(5,6,7,8,9))))

    println(List.foldRight(List(1,2,3), Nil:List[Int])((a,b) => Cons(a+1,b)))
    println(List.foldRight(List(1.0,2.0,3.0), Nil:List[String])((a,b) => Cons(a.toString+'3', b)))
    println(List.map(List(1,2,3))(_ * 2))

    println(List.filter(List(1,2,3))(_ % 2 == 0))
    println(List.flatMap(List(1,2,3))(i => List(i,i)))
    println(List.filterFlatMap(List(1,2,3))(_ % 2 == 0))

    println(List.zipWith(List(1,2,3), List("shoe","bags","books"))(_.toString + _))
    println(List.hasSubsequence(List(1,2,3), List(2, 3, 4)))
  }
}