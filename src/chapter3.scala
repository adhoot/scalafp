package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, l)
    case Cons(h1, t) => Cons(h, t)
  }


  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) {
      dropWhile(t, f)
    } else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => Cons(h, init(t))
    case _ => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,b) => b+1)

  def foldLeft1[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(acc:B, as:List[A]):B = as match {
      case Nil => acc
      case Cons(h, t) => loop(f(acc, h), t)
    }

    loop(z, l)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum_fl(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)

  def product_fl(ds: List[Double]): Double = foldLeft(ds, 1.0)((y:Double,
          x:Double) => if (x == 0.0) 0.0 else y * x)

  def len_fl[A](l: List[A]): Int = foldLeft(l, 0)((b,_) => b+1)

  def reverse_fl2[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def fr_from_fl[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, z)((a, b) => f(b, a))
  }

  def append_fr[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x,
                                                                           as) => Cons(x, as))

  def map[A, B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
object runner {
  def main(args:Array[String]) = {
    println("x is " + List.x)

    val l = List(1, 2, 3, 4, 5, 6);
    val dl = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    println(l)
    println("tail" + List.tail(l))
    println("setHead" + List.setHead(l, -1))
    println("drop -1" + List.drop(l, -1))
    println("drop 0" + List.drop(l, 0))
    println("drop 3" + List.drop(l, 3))
    println("drop 8" + List.drop(l, 8))
    println("dropWhile  < 4 " + List.dropWhile(l, (x:Int) => (x < 4)))
    println("init" + List.init(l))
    println("length" + List.length(l))
    println("foldLeft concat" + List.foldLeft(l, "")((s:String,
                                                  x:Int) => s + x.toString()))
    println("sum" + List.sum(l))
    println("sum" + List.sum2(l))
    println("sum" + List.sum_fl(l))

    println("product" + List.product(dl))
    println("product" + List.product2(dl))
    println("product" + List.product_fl(dl))

    println("length" + List.len_fl(l))

    println("reverse "  + List.reverse_fl2(l))

    println("append " + List.append(l, List(-1, -2, -3)))

  }
}
