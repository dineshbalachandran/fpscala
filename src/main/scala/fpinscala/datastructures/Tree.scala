package fpinscala.datastructures
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(t: Tree[Int]) : Int = {
    def go(s: Tree[Int], acc: Int) : Int = s match {
      case Leaf(v) => v max acc
      case Branch(left, right) => go(left, acc) max go(right, acc)
    }
    go(t, -1)
  }

  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = {
    def loop(s: Tree[A]) : Tree[B] = s match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(loop(left), loop(right))
    }
    loop(t)
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}

object TreeTest {
  def main(args: Array[String]): Unit = {

    val left = Branch(Leaf(10), Leaf(200))
    val right = Branch(Leaf(3), Leaf(4))

    val root = Branch(left, right)

    println(Tree.size(root))
    println(Tree.maximum(root))
    println(Tree.map(root)(_*2))
    println(Tree.fold(root)(x => Leaf(x*2): Tree[Int])(Branch(_,_)))
  }
}
