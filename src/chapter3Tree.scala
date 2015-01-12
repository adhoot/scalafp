sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def countNodes(ints: Tree[Int]): Int = ints match {
    case Leaf(v) => return 1
    case Branch(left, right) => countNodes(left) + countNodes(right)
  }

  def maximum(ints: Tree[Int]): Int = ints match {
    case Leaf(v) => return v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth(ints: Tree[Int]): Int = ints match {
    case Leaf(v) => return 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[T,U](ints: Tree[T])(f: T => U): Tree[U] = ints match {
    case Leaf(v) => return Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[T, U](tree: Tree[T])(f: T => U)(g: (U, U) => U): U = tree match {
    case Leaf(v) => return f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def map_via_fold[T, U](ints: Tree[T])(f: T => U): Tree[U] = {
    fold[T, Tree[U]](ints)((t1: T) => Leaf(f(t1)))((u1, u2) => Branch(u1, u2))
  }

  def depth_via_fold(ints: Tree[Int]) =
    fold(ints)(_ => 1)((d1, d2) => 1 + (d1 max d2))

  def max_via_fold(ints: Tree[Int]) =
    fold(ints)(v => v)((v1, v2) => v1 max v2)

  def count_via_fold(ints: Tree[Int]) =
    fold(ints)(_ => 1)((v1, v2) => v1 + v2)
}

object runner {
  def main(args: Array[String]) = {
    val x = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf
      (5)))))
    println("count nodes is " + Tree.countNodes(x))
    println("max is " + Tree.maximum(x))
    println("depth nodes is " + Tree.depth(x))
    println("count nodes via fold is " + Tree.count_via_fold(x))
    println("max via fold is " + Tree.max_via_fold(x))
    println("depth via fold nodes is " + Tree.depth_via_fold(x))
  }
}