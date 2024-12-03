# frozen_string_literal: true

module Util
  def read_input_lines
    year, day = get_year_day

    day_str = "%02d" % day

    if ARGV.length > 0 && ARGV[0].to_i.is_a?(Numeric)
      example = ARGV[0].to_i
      lines = File.readlines("../inputs/#{year}/day-#{day_str}-example-#{example}.txt")
    else
      lines = File.readlines("../inputs/#{year}/day-#{day_str}-input.txt")
    end
    lines
  end

  def get_year_day
    caller.each do |c|
      m = c.match /.*ruby\/(\d{4})\/day(\d{2})\.rb.*/
      if m
        year = m[1].to_i
        day = m[2].to_i
        return [year, day]
      end
    end
  end
end
