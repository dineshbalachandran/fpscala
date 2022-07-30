package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ =>  this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty)((a,b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => b && p(a))

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]) : Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, b)(s => s match {
    case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
    case _ => None
  })

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map(x => cons(x._1, unfold(x._2)(f))).getOrElse(empty)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  def fibsUnfold(): Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))



}

object StreamTest {
  def main(args: Array[String]) : Unit = {
    val s = Stream(1,2,3,4,5)
//    val t = s.take(3)
//    println(t.toList)
//    println(t.drop(2).toList)
//    println(s.takeWhile(_ % 2 != 0).toList)
//    println(s.takeWhileFold(_ % 2 != 0).toList)
//    println(Stream(1,2).flatMap(x => Stream(x+1, x+2)).toList)
//
//    println(fibsUnfold().take(10).toList)
//    println(fibs().take(10).toList)
//    println(constant("One").take(5).toList)
    println(from(12).take(5).toList)
    println(fromUnfold(12).take(5).toList)


  }
}