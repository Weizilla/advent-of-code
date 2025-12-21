# frozen_string_literal: true

require_relative '../util'

module Year2025
  module Day02
    class Part1
      include Util

      def run
        id_ranges = read_input_lines[0].split(",")

        id_ranges.sum { |r| invalid_ids(r) }
      end

      private

      def invalid_ids(range)
        start = range.split("-")[0].to_i
        stop = range.split("-")[1].to_i

      end
    end

    class Part2
      include Util

      def run

      end
    end
  end
end

if __FILE__ == $0
  p Year2025::Day02::Part1.new.run
end
