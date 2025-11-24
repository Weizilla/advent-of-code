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
end

p Day03::Part1.new.run
