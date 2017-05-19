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

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -5, abs))
    println(formatResult("factorial", 5, factorial))

    val sorting = (x: Int, y: Int) => y > x;
    println(isSorted(Array(1, 2, 3, 4), sorting));

  }
}
