package assignment1

import list.implementation.SinglyLinkedIntList
import list.traits.IntList

/**
  * Complete the following exercises to practice the usage of higher order functions.
  */
object ProblemsApplyHOF {

  /**
    * multiplyAndFilterEven should multiply all elements of the IntList by
    * the factor x and filter all element that are even
    */
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = {
    l.map(a => a * x).filter(b => b % 2 == 0)
  }

  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = {
    l.insertionSort.get(0)
  }

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = {
    l.filter(x => x % 2 != 0).reduceRight(_ + _)
  }

  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = {
    l.filter(x => x % 2 == 0).size
  }
}
