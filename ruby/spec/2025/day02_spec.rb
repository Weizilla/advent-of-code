# frozen_string_literal: true

require_relative '../../2025/day02'

RSpec.describe Year2025::Day02 do
  context "part1 is valid" do
    tests = [
      ["11-22", [11, 22]],
      ["95-115", [99]],
      ["998-1012", [1010]],
      ["1188511880-1188511890", [1188511885]],
      ["222220-222224", [222222]],
      ["1698522-1698528", []],
      ["446443-446449", [446446]],
      ["38593856-38593862", [38593859]],
    ]

    tests.each do |input, expected|
      it 'returns the correct result' do
        p = Year2025::Day02::Part1.new

        actual = p.invalid_ids(input)
        expect(actual).to eq(expected), "Expected #{expected} Actual #{actual}"
      end
    end
  end
end
