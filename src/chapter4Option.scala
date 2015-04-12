package fpinscala.exceptions


import fpinscala.datastructures.Cons

import scala.{Option => _, Some => _, Either => _, _}

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a:A) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a:A) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a:A) => f(a)
    case None => None
  }

  def flatMap1[B](f: A => Option[B]): Option[B] = map(f) getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this map (x => Some(x)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap (a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (ai => b map (bi => f(ai, bi)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case h :: t => map2(h, sequence(t)) ((ha, ta) => ha::ta)
    //case Cons(h: Option[A], t: List[Option[A]]) => map2(h, sequence(t)) ((ha:A, ta:List[A]) => Cons(ha, ta)) orElse None
    //case h :: t => h flatMap (ha => sequence(t) map (ta => ha :: ta))
    case Nil => Some(Nil)
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_::_))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map (f(_)))

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldLeft[Option[List[B]]](Some(Nil))((bs, ai) => map2(f(ai), bs)(_::_))
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse2(t)(f))(_ :: _)
  }

  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case h::t => f(h) match {
      case None => None
      case Some(x) => traverse3(t)(f) map (l => x::l)
    }
    case Nil => Some(Nil)
  }

  def sequence_using_traverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(x => x)
}

object runner {
  def main(args: Array[String]) = {
    val x:Option[Int] = Some(11)
    val nullx:Option[Int] = None
    println("Map Some " + x.map(a => (a + 1).toString()) + " None " + nullx.map(a => (a + 1).toString()))
    println("GetOrElse Some " + x.getOrElse(20) + " None " + nullx.getOrElse(20))
    println("GetOrElse Some(Some) " + Some(nullx).getOrElse(42) + " vs Some " + nullx.getOrElse(42))

    val as:List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val as2:List[Option[Int]] = List(Some(1), None, Some(3))
    println("Sequence " + Option.sequence(as) + " sequence " + Option.sequence(as2))

    val ints = List(1, 2, 3)
    println("Traverse " + Option.traverse2(ints)((a) => if (a == 2) None else if (a == 3) throw new Exception("Error") else Some(a)))
  }
}