use advent_of_code::read_input;
use advent_of_code::Solution;

pub struct Day01 {}

impl Solution for Day01 {
    fn year(&self) -> i32 {
        2022
    }

    fn day(&self) -> i32 {
        1
    }

    fn part1(&self) -> Option<String> {
        let input = read_input(self.year(), self.day()).to_string();
        Some(do_stuff(input))
    }

    fn part2(&self) -> Option<String> {
        None
    }
}

fn do_stuff(input: String) -> String {
    input
}
