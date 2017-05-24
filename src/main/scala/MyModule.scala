import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = {
    if (n > 0) n else -n
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc else go(n - 1, n * acc)
    }

    go(n, 1);
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) {
        return acc;
      }
      go(n - 1, acc, prev + acc)
    }

    go(n - 1, 0, 1);
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) {
        return true;
      } else if (ordered(as(n), as(n + 1))) {
        loop(n + 1);
      } else {
        return false;
      }
    }

    loop(0);
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  // Exercise 3.1 returns 3

  //Exercise 3.2
  
  def removeFirstElement[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => t
  }

  //Exercise 3.3
  // Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  def setHead[A](x: A, l: List[A]) = l match {
    case Nil => List(x)
    case _ :: t => x :: t
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -5, abs))
    println(formatResult("factorial", 5, factorial))

    val sorting = (x: Int, y: Int) => y > x;
    println(isSorted(Array(1, 2, 3, 4), sorting));

    println(removeFirstElement(List(1, 2, 3, 4, 5)))
    println(removeFirstElement(List(1)))
    println(removeFirstElement(List()))

    println(setHead(1, List()))
    println(setHead(1, List(2)))
    println(setHead(1, List(2,3)))
  }
}
