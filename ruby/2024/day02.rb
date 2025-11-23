# frozen_string_literal: true

require_relative '../util'

class Day02
  include Util

  def part1
    lines = read_input_lines
    safes = lines.map do |line|
      safe?(line)
    end

    safes.count { |s| s }
  end

  private

  # @param [String] line
  def safe?(line)
    levels = line.split(' ').map(&:to_i)
    diffs = levels.each_cons(2).map { |a, b| b - a }
    diffs.all? { |d| d.between?(1, 3) } || diffs.all? { |d| d.between?(-3, -1) }
  end
end

puts Day02.new.part1
