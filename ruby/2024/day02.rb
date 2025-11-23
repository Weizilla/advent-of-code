# frozen_string_literal: true

require_relative '../util'

class Day02
  include Util

  def part1
    lines = read_input_lines
    safes = lines.map do |line|
      is_safe?(line)
    end

    safes.count { |s| s }
  end

  private

  # @param [String] line
  def is_safe?(line)
    a = line.split(" ").map { |a| a.to_i }

    diffs = a.each_cons(2)
      .filter { |p|p.length == 2 }
      .map { |pair| pair[1] - pair[0] }

    num_outside_range = diffs.count do |d|
      abs_d = d.abs
      abs_d < 1 || abs_d > 3
    end

    num_increasing = diffs.count do |d|
      d > 0
    end

    num_outside_range == 0 && (num_increasing == diffs.count || num_increasing == 0)
  end

end

puts Day02.new.part1
