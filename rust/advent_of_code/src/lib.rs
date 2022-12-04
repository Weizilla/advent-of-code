use std::fs;

use crate::InputType::{EXAMPLE, INPUT};

pub enum InputType {
    INPUT,
    EXAMPLE(i32),
}

pub struct SolutionArgs {
    pub year: i32,
    pub day: i32,
    pub input_type: InputType,
}

// This is prob bad shoving OOP into rust
// Revisit https://doc.rust-lang.org/stable/book/ch17-00-oop.html
pub trait Solution {
    fn args(&self) -> SolutionArgs;
    fn part1(&self) -> Option<String>;
    fn part2(&self) -> Option<String>;
}

pub fn read_input(args: SolutionArgs) -> String {
    let day = format!("{:02}", args.day);
    let year = args.year;
    let suffix = match args.input_type {
        INPUT => "input".to_string(),
        EXAMPLE(i) => format!("example-{i}"),
    };
    let file_path = format!("../../inputs/{year}/day-{day}-{suffix}.txt");
    let contents = fs::read_to_string(file_path).expect("Error reading input");
    return contents;
}

pub fn read_lines(args: SolutionArgs) -> Vec<String> {
    read_input(args).lines().map(|s| s.to_string()).collect()
}

/*
TEMPLATE
use advent_of_code::{InputType, read_lines, SolutionArgs};
use advent_of_code::Solution;

pub struct Day0Z {}

impl Solution for Day0Z {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: Z,
            input_type: InputType::EXAMPLE(1),
        }
    }

    fn part1(&self) -> Option<String> {
        None
    }

    fn part2(&self) -> Option<String> {
        None
    }
}
**/
