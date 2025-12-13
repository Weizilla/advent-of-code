# frozen_string_literal: true

require_relative '../util'

module Year2024
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

      # @param [String] line
      def safe?(line)
        levels = line.split.map(&:to_i)
        levels_safe?(levels)
      end

      def levels_safe?(levels)
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

      # @param [String] line
      def safe?(line)
        levels = line.split.map(&:to_i)
        return true if Part1.new.levels_safe?(levels)

        levels.each_index do |i|
          n = levels.dup
          n.delete_at(i)
          return true if Part1.new.levels_safe?(n)
        end

        false
      end
    end
  end
end

puts Year2024::Day02::Part2.new.run
