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
      case Nil => Nil
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
    foldRight(reverse(as), z)(f)
  }

  def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)(f)
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
    @tailrec
    def loop[A](as: List[List[A]], res: List[A]): List[A] = as match {
      case Nil => res
      case x :: y :: z => loop(z, append(x, y))
      case x :: Nil => append(res, x)
    }

    loop(as, List[A]())
  }

  //Exercise 3.16
  //  Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List!)
  def plusOne(as: List[Int]): List[Int] = {
    @tailrec
    def loop(as: List[Int], res: List[Int]): List[Int] = as match {
      case Nil => reverse(res)
      case h :: t => loop(t, (h + 1) :: res)
    }

    loop(as, List())
  }

  def plusOneWithMap(as: List[Int]): List[Int] = {
    as.map(n => n + 1)
  }

  //Exercise 3.17
  // Write a function that turns each value in a List[Double] into a String.
  // You can use the expression d.toString to convert some d: Double to a String.
  def listOfDoubleToListOfString(as: List[Double]): List[String] = {
    @tailrec
    def loop(as: List[Double], res: List[String]): List[String] = as match {
      case Nil => reverse(res)
      case h :: t => loop(t, h.toString :: res)
    }

    loop(as, List())
  }

  //Exercise 3.18
  //Write a function map that generalizes modifying each element in a list
  // while maintaining the structure of the list. Here is its signature:[12]
  //In the standard library, map and flatMap are methods of List.
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(as: List[A], res: List[B])(f: A => B): List[B] = as match {
      case Nil => reverse(res)
      case h :: t => loop(t, f(h) :: res)(f)
    }

    loop(as, List())(f)
  }

  //Exercise 3.19
  //Write a function filter that removes elements from a list unless they satisfy a given predicate.
  // Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def loop(as: List[A], res: List[A])(f: A => Boolean): List[A] = as match {
      case Nil => reverse(res)
      case h :: t if f(h) => loop(t, h :: res)(f)
      case _ :: t => loop(t, res)(f)
    }

    loop(as, List())(f)
  }

  //Exercise 3.20
  //Write a function flatMap that works like map except that the function given will return a list instead of a single result,
  // and that list should be inserted into the final resulting list. Here is its signature:
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    @tailrec
    def loop(as: List[A], res: List[B])(f: A => List[B]): List[List[B]] = as match {
      case Nil => List(reverse(res))
      case h :: t => loop(t, append(f(h), res))(f)
    }

    concatenate(loop(as, List())(f))
  }

  //Exercise 3.21
  //Use flatMap to implement filter.
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x: A) => f(x) match {
      case false => List()
      case true => List(x)
    })
  }

  //Exercise 3.22
  //Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  // For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec
    def loop(l1: List[Int], l2: List[Int], res: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, Nil) => reverse(res)
      case (h1 :: t1, h2 :: t2) => loop(t1, t2, (h1 + h2) :: res)
    }

    loop(l1, l2, List())
  }

  //Exercise 3.23
  // Generalize the function you just wrote so that it’s not specific to integers or addition.
  // Name your generalized function zipWith.
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def loop(l1: List[A], l2: List[A], res: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
      case (Nil, Nil) => reverse(res)
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (h1 :: t1, h2 :: t2) => loop(t1, t2, f(h1, h2) :: res)(f)
    }

    loop(l1, l2, List())(f)
  }

  //bit more flexible
  def zipWith2[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def loop(l1: List[A], l2: List[B], res: List[C])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, Nil) => reverse(res)
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (h1 :: t1, h2 :: t2) => loop(t1, t2, f(h1, h2) :: res)(f)
    }

    loop(l1, l2, List())(f)
  }

  //Exercise 3.24
  // Hard: As an example, implement hasSubsequence for
  // checking whether a List contains another List as a subsequence.
  // For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
  // You may have some difficulty finding a concise purely functional implementation that is also
  //           efficient. That’s okay. Implement the function however comes most naturally.
  // We’ll return to this implementation in chapter 5 and hopefully improve on it.
  // Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(sup: List[A], sub: List[A], res: Int): Int = (sup, sub) match {
      // empty sub list
      case (_, Nil) => res
      // empty super list
      case (Nil, _) => res
      // end of sub list iteration
      case (suph :: _, subh :: Nil) if suph == subh => res + 1
      // end of super list iteration
      case (_ :: Nil, _ :: _) => res
      // match: move both lists forward
      case (suph :: supt, subh :: subt) if suph == subh => loop(supt, subt, res + 1)
      // no match: move super list forward
      case (_ :: supt, _ :: _) => loop(supt, sub, 0)
    }

    loop(sup, sub, 0) == length(sub)
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //Exercise 3.25
  //Write a function size that counts the number of nodes (leaves and branches) in a tree.

  def main(args: Array[String]): Unit = {

    assert(hasSubsequence(List(), List()))
    assert(!hasSubsequence(List(), List(1)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 5)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(5)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(4, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(3, 4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1)))
    assert(hasSubsequence(List(1, 2, 3, 4), List()))

    assert(sumLists(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))

    assert(filterWithFlatMap(List(1, 2, 3, 4))(a => a % 2 == 0) == List(2, 4))

    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

    assert(filter(List(1, 2, 3, 4))(a => a % 2 == 0) == List(2, 4))

    assert(map(List(1, 2))(a => a + 1) == List(2, 3))

    assert(listOfDoubleToListOfString(List(0, 1)) == List("0.0", "1.0"))
    assert(listOfDoubleToListOfString(List(0)) == List("0.0"))
    assert(listOfDoubleToListOfString(List()) == List())

    assert(plusOneWithMap(List(0, 1)) == List(1, 2))
    assert(plusOneWithMap(List(0)) == List(1))
    assert(plusOneWithMap(List()) == List())

    assert(plusOne(List(0, 1)) == List(1, 2))
    assert(plusOne(List(0)) == List(1))
    assert(plusOne(List()) == List())

    assert(concatenate(List()) == List())
    assert(concatenate(List(List(), List(), List())) == List())
    assert(concatenate(List(List(), List(), List(1))) == List(1))
    assert(concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))

    assert(append(List(), List(1)) == List(1))
    assert(append(List(1, 2, 3), List(4)) == List(1, 2, 3, 4))

    assert(foldRightInTermsOfFoldLeft(List(1, 2, 3), List[Int]())((a, b) => a :: b) == List(1, 2, 3))
    assert(foldLeftInTermsOfFoldRight(List(1, 2, 3), List[Int]())((a, b) => a :: b) == List(3, 2, 1))

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
