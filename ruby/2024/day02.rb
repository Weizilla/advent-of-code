# frozen_string_literal: true

require_relative '../util'

module Day02
  class Part1
    include Util

    def run
      lines = read_input_lines
      safes = lines.map do |line|
        safe?(line)
      end

      safes.count { |s| s }
    end

    private

    # @param [String] line
    def safe?(line)
      levels = line.split.map(&:to_i)
      diffs = levels.each_cons(2).map { |a, b| b - a }
      diffs.all? { |d| d.between?(1, 3) } || diffs.all? { |d| d.between?(-3, -1) }
    end
  end

  class Part2
    include Util

    def run
      lines = read_input_lines
      safes = lines.map do |line|
        safe?(line)
      end

      safes.count { |s| s }
    end

    private

    # @param [String] line
    def safe?(line)
      levels = line.split.map(&:to_i)
      diffs = levels.each_cons(2).map { |a, b| b - a }
      diffs.count { |d| d.between?(1, 3) } >= diffs.length - 1 ||
        diffs.count { |d| d.between?(-3, -1) } >= diffs.length - 1
    end
  end
end

puts Day02::Part2.new.run
