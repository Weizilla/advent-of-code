use advent_of_code::{InputType, read_lines, SolutionArgs};
use advent_of_code::Solution;

pub struct Day04 {}

impl Solution for Day04 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 4,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let lines = read_lines(self.args());
        let mut num_overlap = 0;
        for line in lines {
            let splits: Vec<&str> = line.split(",").collect();
            let ranges: Vec<Range> = splits.iter().map(|s| to_range(s)).collect();
            if is_overlap(&ranges) {
                num_overlap += 1;
            }
        }
        Some(num_overlap.to_string())
    }

    fn part2(&self) -> Option<String> {
        let lines = read_lines(self.args());
        let mut num_overlap = 0;
        for line in lines {
            let splits: Vec<&str> = line.split(",").collect();
            let ranges: Vec<Range> = splits.iter().map(|s| to_range(s)).collect();
            if is_overlap_2(&ranges) {
                num_overlap += 1;
            }
        }
        Some(num_overlap.to_string())
    }
}

#[derive(Debug)]
struct Range {
    start: i32,
    end: i32,
}

impl Range {
    fn in_range(&self, num: i32) -> bool {
        self.start <= num && num <= self.end
    }
}

fn to_range(input: &str) -> Range {
    let splits: Vec<&str> = input.split("-").collect();
    Range {
        start: splits[0].parse().expect("error"),
        end: splits[1].parse().expect("error"),
    }
}

fn is_overlap(ranges: &Vec<Range>) -> bool {
    let v1 = &ranges[0];
    let v2 = &ranges[1];

    (v1.start <= v2.start && v1.end >= v2.end) || (v2.start <= v1.start && v2.end >= v1.end)
}

fn is_overlap_2(ranges: &Vec<Range>) -> bool {
    let v1 = &ranges[0];
    let v2 = &ranges[1];

    v1.in_range(v2.start) || v1.in_range(v2.end) || v2.in_range(v1.start) || v2.in_range(v1.end)
}
