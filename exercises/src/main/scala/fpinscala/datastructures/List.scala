//package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Nie ma")
      case Cons(_,t) => t
    }
  def head[A](l: List[A]): A =
    l match {
      case Nil => sys.error("Nie ma")
      case Cons(h,_) => h
    }

  def setHead[A](l: List[A], h: A): List[A] = l match
  {
    case Nil => Nil
    case _ => Cons(h,l)
  }
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ => n match {
      case 0 => l
      case _ if n<0 => l
      case _ => drop (tail(l), n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) if !f(h) => l
    case Cons(h,t) if f(h) => dropWhile(t,f)
  }

  /**
    * odwraca listę
    */
  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
      def loop(el:List[A],acc:List[A]):List[A] = {
          el match {
            case Nil => acc
            case Cons(h,t) => loop(t,Cons(h,acc))
        }
      }
      loop(l,Nil)
    
  }
  /**
    * funkcja odwrotna
    */
  def revrf[A,B](f:(A,B)=> B) : (B,A)=>B = {
      (b:B,a:A) => f(a,b)
  }
  def revlf[A,B](f:(B,A)=> B) : (A,B)=>B = {
      (a:A,b:B) => f(b,a)
  }

  def foldRightByFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B ={
       foldLeft(reverse(as),z)(revrf(f))
  }
  def foldLeftByFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B ={
       foldRight(reverse(as),z)(revlf(f))
  }
  /**
    * Zwraca wszystkie elementy oprócz ostatniego
    */
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
      def loop(el:List[A],prev:List[A]):List[A] = { 
        el match {
          case Nil => reverse(prev)
          case Cons(h,Nil) => reverse(prev)
          case _ => loop(tail(el),Cons(head(el),prev))
        }
      }
      loop(l,Nil)
    }



  def length[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(_,Nil) => 1
      case _ => length(tail(l)) + 1
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =  {
      @annotation.tailrec
      def loop(el:List[A],acc:B):B = {
        el match {
          case Nil => acc
          case _ => loop(tail(el), f(acc,head(el)))
        }
      }
      loop(l,z)
  }
  def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B =  {
      l match {
        case Nil => z
        case Cons(h,Nil) => f(z,h)
        case Cons(h,t) => foldLeft(t, f(z,h))(f)
      }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(f(h),map(t)(f))
  }

  def stack() = {
    throw new Exception("boom!")
  }

}

object ListExample {
def main(args: Array[String]): Unit = {
  val x = List(1,2,3,4,5,6,7,8,9,0)
  val y = List("a","b","c","d")
  
  val ex = List.foldRight(x,0)(_ + _)
  val ey = List.foldRight(y,"")(_ concat _)

    //List.foldRightByFoldLeft(x,0)(sum)
}
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

}
