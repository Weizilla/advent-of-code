package com.weizilla.adventofcode
package y2024

import utils.Day

import scala.collection.mutable

class Day05(example: Integer) extends Day(2024, 5, example) {
  override def part1(): Any = {
    val before = mutable.Map[Int, mutable.Set[Int]]()
    val after = mutable.Map[Int, mutable.Set[Int]]()
    val updates = mutable.ListBuffer[List[Int]]()

    for (line <- reader.readLines()) {
      if (line.contains("|")) {
        val splits = line.split("\\|").map(_.toInt)
        val b = splits(0)
        val a = splits(1)
        after.getOrElseUpdate(b, mutable.Set[Int]()).add(a)
        before.getOrElseUpdate(a, mutable.Set[Int]()).add(b)
      } else if (line.nonEmpty) {
        val update: List[Int] = line.split(",").map(_.toInt).toList
        updates.append(update)
      }
    }

    var goodUpdates = mutable.ListBuffer[List[Int]]()
    var sum = 0;
    for (update <- updates) {
      var inAfter = false
      var inBefore = false
      for ((value, index) <- update.zipWithIndex) {
        if (index > 0) {
          val b = update(index - 1)
          inAfter = inAfter || after.get(value).map(_.contains(b)).getOrElse(false)
        }
        if (index < update.length - 1) {
          val a = update(index + 1)
          inBefore = inBefore || before.get(value).map(_.contains(a)).getOrElse(false)
        }
      }
      if (!inBefore && !inAfter) {
        goodUpdates.addOne(update);
        sum += update(update.length / 2)
      }
    }

    print(goodUpdates)

    sum
  }
}
