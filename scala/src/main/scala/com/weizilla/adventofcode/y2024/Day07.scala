package com.weizilla.adventofcode.y2024

import com.weizilla.adventofcode.utils.Day

class Day07(example: Integer) extends Day(2024, 7, example) {
  override def part1(): Any = {
    val lines = reader.readLines()
    val equations = lines.map(parse).toList
    val sum = equations.filter(e => isValid1(e, 0)).map(_.result).sum
    sum
  }

  private def parse(line: String): Equation = {
    val result = line.split(":")(0)
    val n = line.split(":")(1)
    val nums = n.split(" ").filterNot(_.isEmpty).map(_.toLong).toList

    Equation(nums, result.toLong)
  }

  private def isValid1(e: Equation, curr: Long): Boolean = {
    if (e.nums.isEmpty) {
      return e.result == curr
    }

    val next::rest = e.nums
    val sumResult = isValid1(Equation(rest, e.result), curr + next)
    val productResult = isValid1(Equation(rest, e.result), curr * next)
    sumResult || productResult
  }

  override def part2(): Any = {
    val lines = reader.readLines()
    val equations = lines.map(parse).toList
    val sum = equations.filter(e => isValid2(e, 0)).map(_.result).sum
    sum
  }

  private def isValid2(e: Equation, curr: Long): Boolean = {
    if (e.nums.isEmpty) {
      return e.result == curr
    }

    val next::rest = e.nums
    val sumResult = isValid2(Equation(rest, e.result), curr + next)
    val productResult = isValid2(Equation(rest, e.result), curr * next)
    val catResult = isValid2(Equation(rest, e.result), (s"${curr.toString}${next.toString}").toLong)
    sumResult || productResult || catResult
  }

  case class Equation(nums: List[Long], result: Long) { }
}
