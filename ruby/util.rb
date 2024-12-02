# frozen_string_literal: true

module Util
  def read_input_lines
    if ARGV.length > 0 && ARGV[0].to_i.is_a?(Numeric)
      example = ARGV[0].to_i
      lines = File.readlines("../inputs/2024/day-01-example-#{example}.txt")
    else
      lines = File.readlines("../inputs/2024/day-01-input.txt")
    end

    lines
  end
end
