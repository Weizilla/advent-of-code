package com.weizilla.adventofcode
package utils

import org.slf4j.LoggerFactory

class Day(year: Int, day: Int, example: Integer) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  protected val reader = new InputReader(year, day, example)

  def part1(): Any = { }

  def part2(): Any = { }

  def printAlways(input: Any, args: Any *): Unit = {
    if (args.isEmpty) {
      logger.info("{}", input)
    } else {
      logger.info(input.toString, args: _*)
    }
  }

  def print(input: Any, args: Any *): Unit = {
    if (example != null) {
      if (args.isEmpty) {
        printAlways("{}", input)
      } else {
        printAlways(input, args: _*)
      }
    }
  }

  def getYear: Int = {
    year
  }

  def getDay: Int = {
    day
  }
}
