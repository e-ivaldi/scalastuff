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

    go(n, 1)
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) {
        return prev
      }
      go(n, acc, prev + acc)
    }

    go(n, 0, 1)
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
        return true
      } else if (ordered(as(n), as(n + 1))) {
        loop(n + 1)
      } else {
        return false
      }
    }

    loop(0)
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
  def setHead[A](x: A, l: List[A]) = l match {
    case Nil => List(x)
    case _ :: t => x :: t
  }

  //Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ if n < 0 => l
    case _ if n > 0 =>
      l match {
        case Nil => Nil
        case _ :: t => drop(t, n - 1)
      }
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: t if f(h) => dropWhile(t, f)
    case h :: t => h :: dropWhile(t, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: _ :: Nil => l match {
      case x :: _ => List(x)
    }
    case h :: t => h :: init(t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case x :: xs => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)((a, b) => a + b)

  def product2(ints: List[Double]): Double = foldRight(ints, 1.0)((a, b) => a * b)

  // Exercise 3.7
  // It doesn't seem so
  def productTest(ints: List[Double]): Double =
  foldRight(ints, 1.0)((a, b) => a match {
    case 0.0 => println("nan"); Double.NaN
    case _ => println("mul"); a * b
  })

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a, b) => (a, b) match {
      case (Nil, _) if (b == 0) => 0
      case _ => b + 1
    })
  }

  //Exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => val t = f(x, z); foldLeft(xs, t)(f)
  }

  //Exercise 3.11
  //Write sum, product, and a function to compute the length of a list using foldLeft.
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)((a, b) => a + b)

  def product3(ints: List[Double]): Double = foldLeft(ints, 1.0)((a, b) => a * b)

  //Exercise 3.12
  //Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  // See if you can write it using a fold.
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((a, b) => a :: b)
  }

  //Exercise 3.13
  //Hard: Can you write foldLeft in terms of foldRight?
  // How about the other way around? Implementing foldRight via foldLeft is useful because
  // it lets us implement foldRight tail-recursively,
  // which means it works even for large lists without overflowing the stack.
  def foldLeftInTermsOfFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(as: List[A], res: List[A]): B = as match {
      case Nil => foldRight(res, z)(f)
      case h :: t => loop(t, h :: res)
    }

    loop(as, List[A]())
    // or foldRight(as.reverse, z)(f)
  }

  def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(as: List[A], res: List[A]): B = as match {
      case Nil => foldLeft(res, z)(f)
      case h :: t => loop(t, h :: res)
    }

    loop(as, List[A]())
    // or foldLeft(as.reverse, z)(f)
  }

  //Exercise 3.14
  //Implement append in terms of either foldLeft or foldRight.
  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((a, b) => a :: b)
  }

  //Exercise 3.15
  //Hard: Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists.
  // Try to use functions we have already defined.
  def concatenate[A](as: List[List[A]]): List[A] = {
    def loop[A](as: List[List[A]], res: List[A]): List[A] = as match {
      case Nil => res
      case x :: y :: z => loop(z, append(x, y))
      case x :: Nil => append(res, x)
    }

    loop(as, List[A]())
  }


  def main(args: Array[String]): Unit = {

    assert(concatenate(List()) == List())
    assert(concatenate(List(List(), List(), List())) == List())
    assert(concatenate(List(List(), List(), List(1))) == List(1))
    assert(concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))

    assert(append(List(), List(1)) == List(1))
    assert(append(List(1, 2, 3), List(4)) == List(1, 2, 3, 4))

    assert(foldRightInTermsOfFoldLeft(List(1, 2, 3), List[Integer]())((a, b) => a :: b) == List[Integer](1, 2, 3))
    assert(foldLeftInTermsOfFoldRight(List(1, 2, 3), List[Integer]())((a, b) => a :: b) == List[Integer](3, 2, 1))

    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))

    assert(foldLeft(List(), 0)((_, b) => b) == 0)
    assert(foldLeft(List(1), 0)((a, b) => a + b) == 1)
    assert(foldLeft(List(1, 2, 3), 0)((a, b) => a + b) == 6)

    assert(length(List()) == 0)
    assert(length(List(1)) == 1)
    assert(length(List(1, 2)) == 2)
    assert(length(List(1, 2, 3, Nil, 5)) == 5)

    productTest(List(1D, 2D, 3D, 0.0, 5D, 6D, 7D, 8D, 9D, 10D))

    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))

    assert(dropWhile(List(1, 2), (x: Int) => x % 2 == 1) == List(2))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 2) == List(2, 3, 4))

    assert(drop(List(), 100) == List())
    assert(drop(List(1, 2, 3), 2) == List(3))
    assert(drop(List(1, 2, 3), 3) == List())
    assert(drop(List(1, 2, 3), -1) == List(1, 2, 3))

    assert(setHead(1, List()) == List(1))
    assert(setHead(1, List(2)) == List(1))
    assert(setHead(1, List(2, 3)) == List(1, 3))

    assert(removeFirstElement(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
    assert(removeFirstElement(List(1)) == List())
    assert(removeFirstElement(List()) == List())

    val asc = (x: Int, y: Int) => y > x
    assert(isSorted(Array(1, 2, 3, 4), asc))

    println(formatResult("abs", -5, abs))
    println(formatResult("factorial", 5, factorial))

  }
}
