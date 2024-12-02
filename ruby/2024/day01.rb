# frozen_string_literal: true

require_relative "../util"

class Day01
  include Util

  def part1
    left = []
    right = []

    lines = read_input_lines
    lines.each do |line|
      l, r = line.split(" ")
      left << r.to_i
      right << l.to_i
    end

    left.sort!
    right.sort!

    sum = 0
    left.zip(right).each do |a, b|
      sum = sum + (a - b).abs
    end

    sum
  end

  def part2
    left = []
    right = Hash.new(0)

    lines = read_input_lines
    lines.each do |line|
      l, r = line.split(" ")
      left << r.to_i
      right[l.to_i] += 1
    end

    result = 0
    left.each do |l|
      result += l * right[l]
    end

    result
  end
end

puts Day01.new.part2


