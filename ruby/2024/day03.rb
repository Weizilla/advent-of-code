# frozen_string_literal: true

require_relative "../util"

class Day03
  include Util

  def part1
    lines = read_input_lines
    lines.map { |line| process_line(line)}.reduce(0) { | a, b| a + b}
  end

  def part2
    lines = read_input_lines
    lines.map { |line| process_line_2(line)}.reduce(0) { | a, b| a + b}
  end

  def process_line(line)
    match = line.scan /mul\((\d+),(\d+)\)/
    sum = 0
    match.each do |m|
      sum += m[0].to_i * m[1].to_i
    end
    sum
  end

  def process_line_2(line)
    match = line.scan /mul\((\d+),(\d+)\)|(don't\(\))|(do\(\))/
    sum = 0
    add_to_sum = true
    match.each do |m|
      puts m
      if add_to_sum && m[0] && m[1]
        sum += m[0].to_i * m[1].to_i
      end
      if m[2]
        add_to_sum = false
      end
      if m[3]
        add_to_sum = true
      end
    end
    sum
  end

end

puts Day03.new.part2
