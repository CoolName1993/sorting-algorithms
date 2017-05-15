package sorting

/**
  * Worst case performance: O(n log n) Memory: n
  * Best case performance: O(n log n) Memory: n
  */
object MergeSort extends Sorting {

  override def sort(array: Seq[Int]): Seq[Int] = {
    def split(list: Seq[Int]): Seq[Int] = {
      val pivot = list.size / 2
      if(pivot == 0) list // If list cannot be divided in 2 then return the list
      else {
        val (left, right) = list.splitAt(pivot) // Split the list in half
        merge(split(left), split(right)) // Split the halves again and merge
      }
    }
    @scala.annotation.tailrec
    def merge(a: Seq[Int], b: Seq[Int], acc: Seq[Int] = Seq.empty): Seq[Int] = {
      // compare each element of a with each element of b
      (a, b) match {
        case (Nil, _) => acc ++ b // If a is empty then add b
        case (_, Nil) => acc ++ a // If b is empty then add a
        case (aHead :: aTail, bHead :: bTail) =>
          // If a.head is less than b.head then append a.head to the accumulator and pass a.tail and b
          if(aHead < bHead) merge(aTail, b, acc :+ aHead)
          // If b.head is less than a.head then append b.head to the accumulator and pass b.tail and a
          else merge(a, bTail, acc :+ bHead)
      }
    }
    split(array) // Recursively split list and perform merge
  }

}
