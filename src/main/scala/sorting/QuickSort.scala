package sorting

/**
  * Worst case performance: O(n^2^) Memory: n
  * Best case performance: O(n log n) Memory: log n
  */
object QuickSort extends Sorting {

  override def sort(array: Seq[Int]): Seq[Int] = {
    def quicksort(a: Seq[Int]): Seq[Int] = {
      if (a.length < 2) a // If list cannot be divided by 2 and return an integer
      else {
        val pivot = a(a.length / 2) // Select the middle element of the list
        quicksort(a.filter(pivot >)) ++ // Perform quicksort on all elements with a lesser value than the pivot value
        a.filter(pivot ==) ++ // All elements that are equal need not be quicksorted
        quicksort(a.filter(pivot <)) // Perform quicksort on all elements with a greater value than the pivot value
      }
    }
    quicksort(array)
  }

}
