package assignment1

import list.implementation.{Cons, Empty, SinglyLinkedIntList}
import list.traits.IntList

object ProblemsLists {

  /**
    *
    * Given a number i that should be duplicated a number of times
    * returns an IntList that contains the duplicated i
    *
    * E.x. duplicateNum(4,3)
    * -> SinglyLinkedList(4, 4, 4, 4)
    *
    * @param i     number to duplicate
    * @param times number of duplicates
    * @return List of duplicated numbers
    */
  def duplicateNum(i: Int, times: Int): IntList = {

    def helper(list: IntList, i: Int, times: Int): IntList = {
      if (times == 0) {
        list
      } else {
        helper(list.append(i), i, times - 1)
      }
    }

    helper(SinglyLinkedIntList(), i, times)
  }


  /**
    *
    * Given an IntList l that contains even and odd numbers
    * All even numbers of the list should be duplicated an number of times
    * returns an IntList that contains the all duplicated even numbers and the
    * remaining odd numbers in the same order as they occur in the origin list
    *
    * E.x. duplicateEqualNumbers(3,SinglyLinkedList(1,4,3,5,8))
    * -> SinglyLinkedList(1, 4, 4, 4, 3, 5, 8, 8, 8)
    *
    * @param times number of duplicates
    * @param l     IntList that should be processed
    * @return IntList that contains the duplicates and all other nums
    */
  def duplicateEqualNumbers(times: Int, l: IntList): IntList = {

    def helper(dupList: IntList, index: Int): IntList = {
      def duplicate(list: IntList, times: Int): IntList = {
        if (times == 0) {
          list
        } else {
          duplicate(list.append(l.get(index)), times - 1)
        }
      }

      if (index < l.size) {
        l.get(index) match {
          case odd if (odd % 2 != 0) => helper(dupList.append(odd), index + 1)
          case even if (even % 2 == 0) => helper(duplicate(dupList, times), index + 1)
        }
      } else {
        dupList
      }
    }

    helper(SinglyLinkedIntList(), 0)
  }

  /**
    *
    * Given two ordered IntLists l1 and l2 (ascending)
    * The function should merge the two ordered lists in a manner
    * that the result is ordered as well
    *
    * E.x. merge(SinglyLinkedList(3, 5, 7), SinglyLinkedList(1, 3, 5, 8))
    * -> SinglyLinkedList(1, 3, 3, 5, 5, 7, 8)
    *
    * @param l1 IntList in an ascending order
    * @param l2 IntList in an ascending order
    * @return IntList that contains all numbers of both lists in an ascending order
    */
  def merge(l1: IntList, l2: IntList): IntList = {
    l1.prefix(l2).insertionSort
  }

  /**
    *
    * Given an unordered IntList
    * The function should split the List in the middle
    * It returns two separated lists
    * If the size of the origin list is odd, the first resulting list
    * should contain one more element than the second
    *
    * E.x. splitList(SinglyLinkedList(3, 5, 7, 1, 3, 5, 8))
    * -> SinglyLinkedList(3, 5, 7, 1) SinglyLinkedList(3, 5, 8)
    *
    * @param l IntList to split
    * @return A tuple of two IntLists that contains the separated lists
    */
  def splitList(l: IntList): (IntList, IntList) = {
    def helper(list1: IntList, list2: IntList, index: Int): (IntList, IntList) = {
      index match {
        case _ if (index < Math.round(l.size.toFloat / 2)) => helper(list1.append(l.get(index)), list2, index + 1)
        case _ if (index >= Math.round(l.size.toFloat / 2) && index < l.size) => helper(list1, list2.append(l.get(index)), index + 1)
        case _ if (index == l.size) => (list1, list2)
      }
    }

    helper(SinglyLinkedIntList(), SinglyLinkedIntList(), 0)
  }

  /**
    *
    * Given an unordered IntList
    * The function should sort the list using the merge sort algorithm
    *
    * E.x. mergeSort(SinglyLinkedList(3, 5, 7, 1, 3, 5, 8))
    * -> SinglyLinkedList(1, 3, 3, 5, 5, 7, 8)
    * Merge Sort Algorithm: https://de.wikipedia.org/wiki/Mergesort
    *
    * @param l IntList to sort
    * @return Sorted IntList
    */
  def mergeSort(l: IntList): IntList = {
    if (l.size / 2 == 0) l
    else {
      def merge(list: IntList, ys: IntList): IntList =
        (list, ys) match {
          case (Empty, x) => x
          case (y, Empty) => y
          case (Cons(x, xs1), Cons(y, ys1)) =>
            if (x < y) Cons(x, merge(xs1, ys))
            else Cons(y, merge(list, ys1))
        }

      val (left, right) = splitList(l)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  /**
    * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
    * in kilograms.
    * Calculate the maximum weight that fits into the bag.
    *
    * examples:
    * Input:  items      = {4, 8, 5, 4, 2, 1}, Bag Capacity c = 10
    * Output: 10
    * With e.g. {4, 4, 2} , {8, 1, 1} or {5, 4, 1} the maximum could be reached
    *
    * Input:  items       = {3, 3, 4, 4, 8}, Bag Capacity c = 9
    * Output: 8
    * With {4, 4} the maximum could be reached
    *
    * @param capacity the capacity of a bag in kg
    * @param items    weights of the items in kilograms that are available
    * @return maximum filling capacity
    */

  def packProblem(capacity: Int, items: IntList): Int = {
    def knapsack(capacity: Int, items: IntList, index: Int): Int = {
      if (capacity == 0 || index == items.size) {
        0
      }
      else if (items.get(index) > capacity) {
        knapsack(capacity, items, index + 1)
      } else {
        (items.get(index) + knapsack(capacity - items.get(index), items, index + 1)).max(knapsack(capacity, items, index + 1))
      }
    }
    knapsack(capacity, items, 0)
  }


  /**
    * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
    * in grams, calculate how many bags are needed to hold all of the given items.
    *
    * Use recursions for the calculation
    *
    * examples:
    * Input:  weights       = {4, 8, 1, 4, 2, 1}, Bin Capacity c = 10
    * Output: 2
    * We need minimum 2 bins to accommodate all items
    * First bin contains {4, 4, 2} and second bin {8, 1, 1}
    *
    * Input:  weights       = {9, 8, 2, 2, 5, 4}
    * Bin Capacity c = 10
    * Output: 4
    * We need minimum 4 bins to accommodate all items.
    *
    * @param capacity   the capacity of a bag in kg
    * @param itemWeight weights of the items in grams (all item weights must be lower than the capacity because in
    *                   this case they won't fit into the bag
    * @return minimum number of bags required
    */
  def minBagsCount(capacity: Int, itemWeights: IntList): Int = ???


}
