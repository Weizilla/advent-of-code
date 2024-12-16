package com.weizilla.adventofcode
package utils

import scala.io.Source
import scala.util.Using

class InputReader(val year: Int, val day: Int, val example: Integer) {

  def readLines(): List[String] = {
    Using(Source.fromFile(getInputPath)) { reader =>
      reader.getLines().toList
    }.get
  }

  def readGrid(): Grid = {
    val grid = new Grid()
    for ((str, y) <- readLines().zipWithIndex) {
      for ((value, x) <- str.split("").zipWithIndex) {
        grid.add(x, y, value)
      }
    }
    grid
  }

  private def getInputPath = {
    val inputPath = if (example != null) {
      f"../inputs/$year/day-$day%02d-example-$example.txt"
    } else {
      f"../inputs/$year/day-$day%02d-input.txt"
    }
    inputPath
  }

}
