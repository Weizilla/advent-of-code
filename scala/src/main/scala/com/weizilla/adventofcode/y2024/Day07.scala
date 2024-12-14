package com.weizilla.adventofcode.y2024

import com.weizilla.adventofcode.utils.Day

class Day07(example: Integer) extends Day(2024, 7, example) {
  override def part1(): Any = {
    val lines = reader.readLines()
    val equations = lines.map(parse).toList
    val sum = equations.filter(isValid).map(_.result).sum
    sum
  }

  private def parse(line: String): Equation = {
    val result = line.split(":")(0)
    val n = line.split(":")(1)
    val nums = n.split(" ").filterNot(_.isEmpty).map(_.toInt).toList

    Equation(nums, result.toInt)
  }

  private def isValid(e: Equation): Boolean = {
    if (e.nums.sum > e.result) {
      return false
    }
    if (e.nums.product < e.result) {
      return false
    }

    isValidRec(e: Equation)
  }



  case class Equation(nums: List[Int], result: Int) { }
}
