package com.weizilla.adventofcode.y2024

import com.weizilla.adventofcode.utils.{Day, Grid, Point}

import scala.collection.mutable


class Day08(example: Integer) extends Day(2024, 8, example) {

  override def part1(): Any = {
    val grid = reader.readGrid()
    val antennas = grid
      .filter(p => !p._2.equals("."))
      .groupBy(p => p._2)

    val results = antennas.values.flatMap(i => makeAntiNodes(i.toList, grid)).toList.distinct

    print(results)

    results.size
  }

  private def makeAntiNodes(points: List[(Point, String)], grid: Grid): mutable.Set[Point] = {
    val results = mutable.Set[Point]()
    for (x <- points.indices; y <- points.indices if x != y) {
      val r = makeAntiNode(points(x)._1, points(y)._1, grid)
      results.addAll(r)
    }

    results
  }

  private def makeAntiNode(p1: Point, p2: Point, grid: Grid): List[Point] = {
    /*
    (1, 1) (4,5)
      => (4 - 1) + 4, (5 - 1) + 5
      => (1 - 4) + 1, (1 - 5) + 1

     (4, 5), (1, 1)
      => (1 - 4) + 1, (1 - 5) + 1
      => (4 - 1) + 4, (5 - 1) + 5
     */

    val x1 = p2.x - p1.x + p2.x
    val y1 = p2.y - p1.y + p2.y

    val x2 = p1.x - p2.x + p1.x
    val y2 = p1.y - p2.y + p1.y

    val results = List(Point(x1, y1), Point(x2, y2))
      .filter(p => grid.inBounds(p))
//      .filter(p => grid.get(p.x, p.y).getOrElse(".").equals("."))

    print("{} {} {}", p1, p2, results)

    results
  }

  override def part2(): Any = {
    val grid = reader.readGrid()
    val antennas = grid
      .filter(p => !p._2.equals("."))
      .groupBy(p => p._2)

    val results = antennas.values.flatMap(i => makeAntiNodes2(i.toList, grid)).toList.distinct

//    print(results)

    results.foreach(p => grid.set(p, "#"))
    print("\n" ++ grid.prettyPrint())

    results.size
  }

  private def makeAntiNodes2(points: List[(Point, String)], grid: Grid): mutable.Set[Point] = {
    print("Points={}", points)
    val results = mutable.Set[Point]()
    for (x <- points.indices; y <- points.indices if x != y) {
      val r = makeAntiNode2(points(x)._1, points(y)._1, grid)
      results.addAll(r)
    }

    results
  }

  private def makeAntiNode2(p1: Point, p2: Point, grid: Grid): List[Point] = {
    /*
    (1, 1) (4,5)
      => (4 - 1) + 4, (5 - 1) + 5
      => (1 - 4) + 1, (1 - 5) + 1

     (4, 5), (1, 1)
      => (1 - 4) + 1, (1 - 5) + 1
      => (4 - 1) + 4, (5 - 1) + 5
     */

    val results = mutable.ListBuffer[Point]()


    var i = 0
    var x1 = (i * (p2.x - p1.x)) + p2.x
    var y1 = (i * (p2.y - p1.y)) + p2.y
    var curr = Point(x1, y1)
    while (grid.inBounds(curr)) {
      results.addOne(curr)

      i += 1
      x1 = (i * (p2.x - p1.x)) + p2.x
      y1 = (i * (p2.y - p1.y)) + p2.y
      curr = Point(x1, y1)
    }

    i = 0
    x1 = (i * (p1.x - p2.x)) + p1.x
    y1 = (i * (p1.y - p2.y)) + p1.y
    curr = Point(x1, y1)
    while (grid.inBounds(curr)) {
      results.addOne(curr)

      i += 1
      x1 = (i * (p1.x - p2.x)) + p1.x
      y1 = (i * (p1.y - p2.y)) + p1.y
      curr = Point(x1, y1)
    }

    print("p1={} p2={} {}", p1, p2, results)

    results.toList
  }
}
