package com.weizilla.adventofcode.utils

import scala.collection.mutable

class Grid extends mutable.Iterable[(Point, String)] {
  private val grid = mutable.Map[Point, String]()
  var min: Point = Point(0, 0);
  var max: Point = Point(0, 0);

  def add(x: Int, y: Int, value: String): Unit = {
    min = Point(math.min(x, min.x), Math.min(y, min.y))
    max = Point(math.max(x, max.x), Math.max(y, max.y))
    grid(Point(x, y)) = value
  }

  def get(x: Int, y: Int): Option[String] = {
    grid.get(Point(x, y))
  }

  def set(point: Point, value: String): Unit = {
    grid.put(point, value)
  }

  def inBounds(point: Point): Boolean = {
    min.x <= point.x && point.x <= max.x && min.y <= point.y && point.y <= max.y
  }

  def prettyPrint(): String = {
    val b = new StringBuilder()

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val value = grid.getOrElse(Point(x, y), " ")
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
