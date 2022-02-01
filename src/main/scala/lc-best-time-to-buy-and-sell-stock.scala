/*
You are given an array prices where prices[i] is the price of a given stock on
the ith day.

You want to maximize your profit by choosing a single day to buy one stock and
choosing a different day in the future to sell that stock.

Return the maximum profit you can achieve from this transaction. If you cannot
achieve any profit, return 0.
 */
package lc

import scala.collection.mutable;

object BestTimeToBuyAndSellSolution {

  object State extends Enumeration {
    val NeverHad, Have, Had = Value
  }

  def maxProfit(prices: Array[Int]): Int = {
    // Every day we may:
    // - Buy, if we have never bought before (NeverHad -> Have)
    // - Do nothing
    // - Sell, if we have bought on a previous day (Have -> Had)
    //
    // Define profit(state) to be the profit we have made so far given that we
    // have just transitioned into `state`.
    //
    import State._

    val profit = mutable.Map(
      NeverHad -> 0.0,
      Have -> Double.NegativeInfinity,
      Had -> Double.NegativeInfinity
    )

    for (p <- prices) {
      profit(Have) = profit(Have).max(profit(NeverHad) - p)
      profit(Had) = profit(Had).max(profit(Have) + p)
    }

    profit(Had).toInt
  }
}
