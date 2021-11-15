package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match {

    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))
  }

  override def size: Int = this match {

    case Empty => 0
    case _ => 1 + tail.size
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = this match {
    case Empty => Empty
    case Cons(head, tail) => Cons(mapFunc(head), tail.map(mapFunc))
  }

  override def filter(filterFunc: Int => Boolean): IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) =>

        if (filterFunc(head)) {
          Cons(head, tail.filter(filterFunc))
        } else tail.filter(filterFunc)
    }
  }

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int =
    this match {
      case Empty => initial
      case Cons(h, t) => t.foldLeft(reduceFunc(initial, h))(reduceFunc)
    }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Cons(head, Empty) => head
      case Cons(head1, Cons(head2, tail)) => Cons(reduceFunc(head1, head2), tail).reduceLeft(reduceFunc)
    }
  }

  override def forAll(predicateFunc: Int => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(head, tail) => if (predicateFunc(head)) {
        tail.forAll(predicateFunc)
      }
      else false
    }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Cons(_, Empty) => reduceFunc(head, initial)
      case Cons(head, tail) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
    }
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match {
    case Cons(_, Empty) => head
    case Empty => 0
    case Cons(head, tail) => reduceFunc(head, tail.reduceRight(reduceFunc))
  }

  override def insertionSort: IntList = {
    def insert(x: Int, list: IntList): IntList = list match {
      case Empty => Cons(x, Empty)
      case Cons(h, t) => if (x <= h) Cons(x, list) else Cons(h, insert(x, t))
    }

    def sort(list: IntList): IntList = list match {
      case Empty => Empty
      case Cons(h, t) => insert(h, sort(t))
    }

    sort(this)
  }

  override def insertSorted(elem: Int): IntList =
    this match {
      case Cons(_, Empty) => Cons(head, Cons(elem, Empty))
      case _ if (head > elem) => Cons(elem, Cons(head, tail))
      case _ if (tail.head > elem) => Cons(head, Cons(elem, tail))
      case _ if (head < elem) => Cons(head, tail.insertSorted(elem))
    }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A =
    this match {
      case Empty => initial
      case Cons(h, t) => t.foldLeft(reduceFunc(initial, h))(reduceFunc)
    }
}
