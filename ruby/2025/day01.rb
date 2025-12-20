# frozen_string_literal: true

require_relative '../util'

module Year2025
  module Day01
    class Part1
      include Util

      def run
        position = 50

        lines = read_input_lines
        lines.count do |l|
          delta = l[1..].to_i
          delta *= -1 if l.start_with?("L")

          position = (position + delta) % 100
          position.zero?
        end
      end
    end

    class Part2
      include Util

      def run(input = nil)
        position = 50

        lines = input || read_input_lines
        lines.sum do |l|
          delta = l[1..].to_i
          delta *= -1 if l.start_with?("L")

          prev_pos = position
          new_pos = position + delta
          position = new_pos % 100

          num_zeros = 0
          if prev_pos.positive? != new_pos.positive? && !prev_pos.zero? && !new_pos.zero?
            num_zeros += 1
          end

          if position.zero?
            num_zeros += 1
          end

          num_zeros += ((position + delta) / 100.0).to_f.abs.to_i

          puts "#{delta} #{position} #{num_zeros}"

          num_zeros
        end
      end
    end
  end
end

if __FILE__ == $0
  p Year2025::Day01::Part2.new.run
end
