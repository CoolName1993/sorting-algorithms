package sorting

import testutils.UnitTest

class MergeSortSpec extends UnitTest {

  "sort" must {

    "sort a sequence of integers" in {
      val sequence = Seq(10,9,8,7,6,5,4,3,2,1,0)
      val result = MergeSort.sort(sequence)
      result mustBe Seq(0,1,2,3,4,5,6,7,8,9,10)
    }

    "sort another sequence of integers" in {
      val sequence = Seq(10,5,9,4,8,1,7,0,6,1,5,5,4,2,3,7,2,1,1,0)
      val result = MergeSort.sort(sequence)
      result mustBe  Seq(0,0,1,1,1,1,2,2,3,4,4,5,5,5,6,7,7,8,9,10)
    }

    "sort a large sequence of integers" in {
      val sequence = 2000000 to 0 by -1
      val result = QuickSort.sort(sequence)
      result mustBe  (0 to 2000000)
    }

  }

}
