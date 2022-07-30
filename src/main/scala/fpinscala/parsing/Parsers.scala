package fpinscala.parsing

import fpinscala.testing._

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) = ParserOps(f(a))

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)
  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List()) else map2(p, listOfN(n-1, p))(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for {a <- p; b <- p2 } yield f(a,b)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = map2(p, p2)((_,_))
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)

    def **[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B>:A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.skip(line-1).findFirst.orElse("")
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object ParserTest {

  def main(args: Array[String]): Unit = {

  }
}

