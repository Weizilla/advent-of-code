# frozen_string_literal: true

require_relative '../util'

module Day03
  class Part1
    include Util

    def run
      line = read_input_lines.join

      match = line.scan(/mul\((\d{1,3}),(\d{1,3})\)/)
      match.sum { |a, b| a.to_i * b.to_i }
    end
  end

  class Part2
    include Util

    def run
      line = read_input_lines.join

      sum = 0
      accept = true

      match = line.scan(/mul\((\d{1,3}),(\d{1,3})\)|(don't\(\))|(do\(\))/)
      match.each do |num1, num2, donot_found, do_found|
        if num1 && num2
          sum += num1.to_i * num2.to_i if accept
        elsif do_found
          accept = true
        elsif donot_found
          accept = false
        else
          raise(StandardError)
        end
      end

      sum
    end
  end
end

p Day03::Part2.new.run
