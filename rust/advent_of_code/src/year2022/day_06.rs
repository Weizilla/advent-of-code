use std::collections::HashSet;

use advent_of_code::{InputType, read_lines, SolutionArgs};
use advent_of_code::Solution;

pub struct Day06 {}

impl Solution for Day06 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 6,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let input = &read_lines(self.args())[0];
        let chars = input.chars();
        let mut prev_chars: Vec<char> = Vec::new();

        for (i, char) in chars.enumerate() {
            if prev_chars.len() == 4 {
                prev_chars.remove(0);
            }
            prev_chars.push(char);

            let unique: HashSet<&char> = HashSet::from_iter(&prev_chars);
            if unique.len() == prev_chars.len() && prev_chars.len() == 4 {
                return Some((i + 1).to_string());
            }
        }
        None
    }

    fn part2(&self) -> Option<String> {
        let input = &read_lines(self.args())[0];
        let chars = input.chars();
        let mut prev_chars: Vec<char> = Vec::new();

        for (i, char) in chars.enumerate() {
            if prev_chars.len() == 14 {
                prev_chars.remove(0);
            }
            prev_chars.push(char);

            let unique: HashSet<&char> = HashSet::from_iter(&prev_chars);
            if unique.len() == prev_chars.len() && prev_chars.len() == 14 {
                return Some((i + 1).to_string());
            }
        }
        None
    }
}
