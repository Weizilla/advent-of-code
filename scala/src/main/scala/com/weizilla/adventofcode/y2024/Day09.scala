package com.weizilla.adventofcode.y2024

import com.weizilla.adventofcode.utils.Day

import scala.collection.mutable

class Day09(example: Integer) extends Day(2024, 9, example) {
  override def part1(): Any = {
    val line = reader.readLines().head

    val blocks = mutable.ListBuffer[Block]()
    var id = 0
    var index = 0
    for ((value, i) <- line.split("").map(_.toInt).zipWithIndex if value != 0) {
      if (i % 2 == 0) {
        val block = FileBlock(index, value, id)
        blocks.addOne(block)
        id += 1
      } else {
        val block = FreeBlock(index, value)
        blocks.addOne(block)
      }
      index += value
    }

    blocks.sorted

    blocks
  }

  private trait Block extends Ordered[Block] {
    def start: Int
    def length: Int
    def end: Int = { start + length }

    override def compare(that: Block): Int = {
      start.compare(that.start)
    }
  }

  private case class FileBlock(start: Int, length: Int, id: Int) extends Block with Ordered[Block] {
  }

  private case class FreeBlock(start: Int, length: Int) extends Block { }
}
