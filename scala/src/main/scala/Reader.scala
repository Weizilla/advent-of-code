package com.weizilla.adventofcode

import scala.io.Source
import scala.util.Using

class Reader {
  def readLines(day: Int, example: Int = 0): List[String] = {
    val fileName = if (example == 0) "input" else s"example-$example"

    Using(Source.fromFile(f"../inputs/2024/day-$day%02d-$fileName.txt")) { reader =>
      reader.getLines().toList
    }.get
  }
}
