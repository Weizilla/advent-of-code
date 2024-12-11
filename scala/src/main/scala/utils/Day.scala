package com.weizilla.adventofcode
package utils

import org.slf4j.{Logger, LoggerFactory}

class Day(year: Int, day: Int, example: Integer) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  protected val reader = new InputReader(year, day, example)

  def part1(): Any = { }

  def part2(): Any = { }

  def printAlways(input: Any): Unit = {
    printAlways("{}", input)
  }

  def printAlways(input: String, args: Any *): Unit = {
    logger.info(input, args)
  }

  def print(input: Any): Unit = {
    print("{}", input)
  }

  def print(input: String, args: Any *): Unit = {
    if (example != null) {
      printAlways(input, args)
    }
  }

  def getYear: Int = {
    year
  }

  def getDay: Int = {
    day
  }
}
