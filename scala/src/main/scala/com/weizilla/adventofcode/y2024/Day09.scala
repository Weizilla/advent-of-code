package com.weizilla.adventofcode.y2024

import com.weizilla.adventofcode.utils.Day

import scala.collection.mutable

class Day09(example: Integer) extends Day(2024, 9, example) {
  override def part1(): Any = {
    val line = reader.readLines().head

    val fileBlocks = mutable.ArrayDeque[FileBlock]()
    val freeBlocks = mutable.ArrayDeque[FreeBlock]()

    var id = 0
    var index = 0
    for ((value, i) <- line.split("").map(_.toInt).zipWithIndex if value != 0) {
      if (i % 2 == 0) {
        val block = FileBlock(index, value, id)
        fileBlocks.addOne(block)
        id += 1
      } else {
        val block = FreeBlock(index, value)
        freeBlocks.addOne(block)
      }
      index += value
    }


    while (freeBlocks.nonEmpty) {
      val free = freeBlocks.removeHead()
      val file = fileBlocks.removeLast()

      if (free.length < file.length) {
        val diff = file.length - free.length

        val newFile = FileBlock(free.start, free.length, file.id)
        fileBlocks.addOne(newFile)

        val rem = FileBlock(file.start, diff, file.id)
        fileBlocks.addOne(rem)

      } else if (free.length > file.length) {
        val diff = free.length - file.length

        val newFile = FileBlock(free.start, file.length, file.id)
        fileBlocks.addOne(newFile)

        val newFree = FreeBlock(free.start + file.length, diff)
        freeBlocks.addOne(newFree)

      } else {
        val newBlock = FileBlock(free.start, free.length, file.id)
        fileBlocks.addOne(newBlock)
      }
      fileBlocks.sortInPlace()
      freeBlocks.sortInPlace()
    }

    val compacted = mutable.ArrayDeque[FileBlock]()
    var end = 0
    for (block <- fileBlocks) {
      compacted.addOne(FileBlock(end, block.length, block.id))
      end += block.length
    }

    print(compacted)

    compacted.map(_.checksum()).sum

  }

  override def part2(): Any = {
    val line = reader.readLines().head

    val fileBlocks = mutable.TreeMap[Int, FileBlock]()
    val freeBlocks = mutable.ArrayDeque[FreeBlock]()

    var id = 0
    var index = 0
    for ((value, i) <- line.split("").map(_.toInt).zipWithIndex if value != 0) {
      if (i % 2 == 0) {
        val block = FileBlock(index, value, id)
        fileBlocks(id) = block
        id += 1
      } else {
        val block = FreeBlock(index, value)
        freeBlocks.addOne(block)
      }
      index += value
    }

    for (fileId <- fileBlocks.keys.toList.reverse) {
      val file = fileBlocks(fileId)
      freeBlocks.sortInPlace()
      val maybeFree = freeBlocks.removeFirst(f => f.length >= file.length && f.start < file.start)
      if (maybeFree.nonEmpty) {
        val free = maybeFree.get
        val diff = free.length - file.length
        if (diff > 0) {
          freeBlocks.addOne(FreeBlock(free.start + file.length, diff))
          freeBlocks.sortInPlace()
        }

        fileBlocks(fileId) = FileBlock(free.start, file.length, file.id)

        print(fileBlocks)
      }
    }

    print(fileBlocks)

    fileBlocks.values.map(_.checksum()).sum
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
    def checksum(): Long = {
      // 10 + 11 + 12 + 13 + 15 + 16 = (10 + 13) * (len / 2)
      // 10 + 11 + 12 + 13 + 14 + 16 + 17 = (10 + 14) * ((len / 2) + 0.5) +

      val maybeHalf = if (length % 2 == 1) 0.5 else 0.0
      val blockNums = ((start + (end - 1)) * ((length / 2) + maybeHalf)).toLong
      blockNums * id
    }

    override def toString: String = {
      s"$start=>$end($id)[${checksum()}]"
    }
  }

  private case class FreeBlock(start: Int, length: Int) extends Block { }
}
