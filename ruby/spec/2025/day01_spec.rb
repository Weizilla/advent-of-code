# frozen_string_literal: true

require_relative '../../2025/day01'

RSpec.describe Year2025::Day01::Part2 do
  context "part2" do
    tests = [
      [50, ["L10"], 0],
      [50, ["L60"], 1],
      [50, ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"], 6],
      [50, ["L100"], 1],
      [50, ["R100"], 1],
      [50, ["L200"], 2],
      [50, ["R200"], 2],
      [50, ["L200", "R200", "L200"], 6],
      [50, ["L50"], 1],
      [50, ["R50"], 1],
      [50, ["R50", "L50", "R50"], 2],
      [1, ["L2", "R2", "L2", "R2"], 4]
    ]

    tests.each do |start, input, expected|
      it "makes answers" do
        p = Year2025::Day01::Part2.new

        actual = p.run(start, input)
        expect(actual).to eq(expected), "Failed for input #{input}. Expected #{expected} Actual #{actual}"
      end
    end
  end
end
