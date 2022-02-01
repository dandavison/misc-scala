/*
  https://leetcode.com/problems/subarray-sum-equals-k

  Given an array of integers nums and an integer k, return the total number of
  continuous subarrays whose sum equals to k.
 */
import scala.collection.mutable;

class Solution {}

object Solution {
  def subarraySum(nums: Array[Int], k: Int): Int = {
    // Suppose partialSums[i] = s. Then we add to the count the count of partial
    // sums to the left that have the value n - k, since each of these marks the
    // beginning of a subarray with sum (n - (n - k)) = k.
    var partialSums = mutable.Map[Int, Int]().withDefaultValue(0)
    var count = 0
    var s = 0
    for (n <- nums) {
      partialSums(s) = partialSums(s) + 1
      s += n
      count += partialSums(s - k)
    }
    count
  }
}
