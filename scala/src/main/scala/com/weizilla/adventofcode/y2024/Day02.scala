package com.weizilla.adventofcode
package y2024

import utils.Day

class Day02(example: Integer) extends Day(2024, 2, example) {

  override def part1(): Any = {
    val lines = reader.readLines()

    val numSafe = lines
      .map(l => l.split(" ").map(s => s.toInt).toList)
      .count(l => isSafe(l))
    return numSafe
  }

  private def isSafe(nums: List[Int]): Boolean = {
    val diffs = nums.zip(nums.tail).map{case (a, b) => a - b}
    val allSameDirection = diffs.forall(a => a > 0) || diffs.forall(a => a < 0)
    val allInRange = diffs.map(a => a.abs).forall(a => a >= 1 && a <= 3)

    allSameDirection && allInRange
  }

  override def part2(): Any = {
    val lines = reader.readLines()

    val numSafe = lines
      .map(l => l.split(" ").map(s => s.toInt).toList)
      .count(l => isSafe2(l))
    return numSafe
  }

  private def isSafe2(nums: List[Int]): Boolean = {
    if (isSafe(nums)) {
      return true;
    }

    return (0 to nums.size).exists(i => isSafeSkip(nums, i))
  }

  private def isSafeSkip(nums: List[Int], skip: Int): Boolean = {
    val input = nums.zipWithIndex.filter(_._2 != skip).map(_._1)
    val diffs = input.zip(input.tail).map{case (a, b) => a - b}
    val allSameDirection = diffs.forall(a => a > 0) || diffs.forall(a => a < 0)
    val allInRange = diffs.map(a => a.abs).forall(a => a >= 1 && a <= 3)

    allSameDirection && allInRange
  }
}
