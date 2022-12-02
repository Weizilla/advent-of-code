use advent_of_code::{InputType, read_input, SolutionArgs};
use advent_of_code::Solution;

pub struct Day01 {}

impl Solution for Day01 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 1,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let input = read_input(self.args()).to_string();
        let lines: Vec<&str> = input.lines().collect();

        let mut cals = 0;
        let mut all_cals: Vec<i32> = Vec::new();
        for line in lines {
            match line {
                "" => {
                    all_cals.push(cals);
                    cals = 0
                }
                _ => {
                    let num: i32 = line.parse().expect("error parsing");
                    cals += num;
                }
            }
        }

        all_cals.push(cals);
        all_cals.sort();

        return Some(all_cals.get(all_cals.len() - 1).expect("not found").to_string());
    }

    fn part2(&self) -> Option<String> {
        let input = read_input(self.args()).to_string();
        let lines: Vec<&str> = input.lines().collect();

        let mut cals = 0;
        let mut all_cals: Vec<i32> = Vec::new();
        for line in lines {
            match line {
                "" => {
                    all_cals.push(cals);
                    cals = 0
                }
                _ => {
                    let num: i32 = line.parse().expect("error parsing");
                    cals += num;
                }
            }
        }

        all_cals.push(cals);
        all_cals.sort();

        let top_3 = &all_cals[all_cals.len() - 3..];

        let sum: i32 = top_3.iter().sum();
        return Some(sum.to_string());
    }
}
