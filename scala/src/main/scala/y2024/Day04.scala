package com.weizilla.adventofcode
package y2024

import utils.Day

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

class Day04 (example: Integer) extends Day(2024, 4, example) {
  override def part1(): Any = {
    val grid = new Grid()
    var foundGrid = new Grid();

    for ((str, y) <- reader.readLines().zipWithIndex) {
      for ((value, x) <- str.split("").zipWithIndex) {
        grid.add(x, y, value)
      }
    }

    print("\n{}", grid.prettyPrint())

    val count = grid.map(t => numValid(t._1, t._2, grid, foundGrid)).sum

    print("\n{}", foundGrid)
    print("\n{}", foundGrid.prettyPrint())

    count
  }

  private def numValid(point: Point, value: String, grid: Grid, foundGrid: Grid): Int = {
    if (!value.equals("X")) {
      return 0;
    }

    val letters: List[String] = List("M", "A", "S")

    var numValid = 0
    for (dy <- -1 to 1; dx <- -1 to 1 if !(dx == 0 && dy == 0)) {
      var valid = true
      var points = List(point)
      for ((expected, num) <- letters.zipWithIndex) {
        val x = point.x + (dx * (num + 1))
        val y = point.y + (dy * (num + 1))
        points = points :+ Point(x, y)
        val actual = grid.get(x, y).getOrElse("")
        if (!actual.equals(expected)) {
          valid = false;
        }
      }

      if (valid) {
        points.foreach(p => foundGrid.add(p.x, p.y, grid.get(p.x, p.y).get))
        numValid += 1
      }
    }

    numValid
  }

  override def part2(): Any = {
    val grid = new Grid()
    var foundGrid = new Grid();

    for ((str, y) <- reader.readLines().zipWithIndex) {
      for ((value, x) <- str.split("").zipWithIndex) {
        grid.add(x, y, value)
      }
    }

    print("\n{}", grid.prettyPrint())

    val count = grid.count(t => numValid2(t._1, t._2, grid, foundGrid))

    print("\n{}", foundGrid)
    print("\n{}", foundGrid.prettyPrint())

    count
  }

  private def numValid2(point: Point, value: String, grid: Grid, foundGrid: Grid): Boolean = {
    if (!value.equals("A")) {
      return false
    }

    val letters: List[String] = List("M", "A", "S")

    var numValid = 0
    for (dy <- -1 to 1; dx <- -1 to 1 if !(dx == 0 || dy == 0)) {
      var valid = true
      var points = List(point)
      for ((expected, num) <- letters.zipWithIndex) {
        val x = point.x + (dx * (num - 1))
        val y = point.y + (dy * (num - 1))
        points = points :+ Point(x, y)
        val actual = grid.get(x, y).getOrElse("")
        if (!actual.equals(expected)) {
          valid = false;
        }
      }

      if (valid) {
        numValid += 1
      }
    }

    numValid == 2
  }}

class Grid extends mutable.Iterable[(Point, String)] {
  private val grid = mutable.Map[Point, String]()
  var min: Point = Point(0, 0);
  var max: Point = Point(0, 0);

  def add(x: Int, y: Int, value: String): Unit = {
    min = Point(math.min(x, min.x), Math.min(y, min.y))
    max = Point(math.max(x, max.x), Math.max(y, max.y))
    grid(Point(x, y)) = value
//    print(grid(Point(x, y)))
  }

  def get(x: Int, y: Int): Option[String] = {
    grid.get(Point(x, y))
  }

  def prettyPrint(): String = {
    val b = new StringBuilder()

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val value = grid.get(Point(x, y)).getOrElse(" ")
        b.append(value)
      }
      b.append("\n")
    }

    b.toString()
  }

  override def iterator: Iterator[(Point, String)] = grid.iterator

  override def toString: String = {
    val keys = grid.keys.toList.sorted

    keys.map(key => s"$key=${grid(key)}").mkString(",")
  }
}

case class Point(x: Int, y: Int) extends Ordered[Point] {

  override def compare(that: Point): Int = (x, y) compare (that.x, that.y)
}
