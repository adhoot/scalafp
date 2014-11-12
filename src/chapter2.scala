/**
 * Created by adhoot on 10/11/14.
 */
object chapter2 {

  object o1 {

  }

  def fib_1(n: Int): Int = {
    // @annotation.tailrec
    def go(n: Int): Int = {
      if (n == 1) 1
      else if (n == 0) 0
      else go(n - 1) + go(n - 2)
    }

    go(n)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, prevminus1: Int): Int = {
      if (n == 1) prev
      else go(n - 1, prev + prevminus1, prev)
    }

    go(n, 1, 0)
  }

  def isSorted[A](as:Array[A], ordered:(A, A) => Boolean) : Boolean = {
    @annotation.tailrec
    def go(n:Int): Boolean = {
      if (n >= as.length) true
      if (!(ordered(as(n), as(n+1)))) false
      else go(n)
    }

    go(0)
  }

  def partial2[A, B, C](a:A, f:(A, B) => C): B => C = {
    def g(b:B): C = {
      f(a,b)
    }

    g
  }

  def partial[A, B, C](a:A, f:(A, B) => C): B => C = {
    (b:B) => f(a,b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a:A) => (b:B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B=>C, g: A=>B) : A => C = {
    (a:A) => f(g(a))
  }

  // def findFirtst[A] (as:Array[A], f)

  def main(args: Array[String]):Unit = {
    //throw new Exception()

    val ox =  o1

    //sqroot(10)(30)
    println(fib(7))
    println(fib(7))
    println(isSorted(Array(1, 2, 3), (a:Int, b:Int) => a < b))
  }
}
