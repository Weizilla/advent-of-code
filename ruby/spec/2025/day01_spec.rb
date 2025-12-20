# frozen_string_literal: true

require_relative '../../2025/day01'

RSpec.describe Year2025::Day01::Part2 do
  context "part2" do
    tests = [
      [["L10"], 0],
      [["L60"], 1],
      [["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"], 6],
    ]

    tests.each do |input, expected|
      it "makes answers" do
        p = Year2025::Day01::Part2.new

        actual = p.run(input)
        expect(actual).to eq(expected), "Failed for input #{input}. Expected #{expected} Actual #{actual}"
      end
    end
  end
end
