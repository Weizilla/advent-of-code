#![allow(dead_code, unused_variables)]

use advent_of_code::{InputType, read_input, SolutionArgs};
use advent_of_code::Solution;

use crate::year2016::day_01::Direction::{E, N, S, W};

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
        let splits: Vec<String> = input.split(",")
            .map(|x| x.trim().to_string())
            .collect();
        println!("{:?}", splits);

        return None;
    }

    fn part2(&self) -> Option<String> {
        None
    }
}

enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    fn turn_left(&self) -> Direction {
        match self {
            N => W,
            E => N,
            S => E,
            W => S,
        }
    }

    fn turn_right(&self) -> Direction {
        match self {
            N => E,
            E => S,
            S => W,
            W => N,
        }
    }
}
