# frozen_string_literal: true

require_relative '../util'

module Year2024
  module Day01
    class Part1
      include Util

      def run
        curr = 50
        num_zeros = 0

        lines = read_input_lines
        lines.each do |l|
          num = l[1..].to_i
          if l.start_with?("L")
            num *= -1
          end
          curr += num
          while curr >= 100
            curr -= 100
          end
          while curr < 0
            curr += 100
          end
          if curr == 0
            num_zeros += 1
          end
        end
        num_zeros
      end
    end

    class Part2
      include Util

      def run
        curr = 50
        num_zeros = 0

        lines = read_input_lines
        lines.each do |l|
          num = l[1..].to_i
          if l.start_with?("L")
            num *= -1
          end
          prev = curr
          curr += num
          while curr >= 100
            curr -= 100
          end
          while curr < 0
            curr += 100
          end
          if curr == 0
            num_zeros += 1
          end
        end
        num_zeros
      end
    end
  end
end

p Year2024::Day01::Part2.new.run
