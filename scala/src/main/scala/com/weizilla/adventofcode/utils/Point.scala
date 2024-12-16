package com.weizilla.adventofcode.utils

import scala.math.Ordered.orderingToOrdered

case class Point(x: Int, y: Int) extends Ordered[Point] {

  override def compare(that: Point): Int = (x, y) compare(that.x, that.y)
}
