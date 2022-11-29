use advent_of_code::{InputType, read_input, SolutionArgs};
use advent_of_code::Solution;

pub struct Day01 {}

impl Solution for Day01 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2016,
            day: 1,
            input_type: InputType::EXAMPLE(1),
        }
    }


    fn part1(&self) -> Option<String> {
        let input = read_input(self.args());
        return Some(do_stuff(&input));
    }

    fn part2(&self) -> Option<String> {
        None
    }
}

fn do_stuff(input: &String) -> String {
    input.to_string()
}
