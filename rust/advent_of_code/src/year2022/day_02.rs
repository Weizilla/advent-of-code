use advent_of_code::{InputType, read_input, SolutionArgs};
use advent_of_code::Solution;

pub struct Day02 {}

impl Solution for Day02 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 2,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let input = read_input(self.args());
        let lines: Vec<&str> = input.lines().collect();

        let mut sum = 0;

        for line in lines {
            if line.len() > 0 {
                let r: Vec<&str> = line.split(" ").collect();
                let round: (&str, &str) = (r[0], r[1]);

                let points = match round {
                    ("A", "X") => 1 + 3,
                    ("A", "Y") => 2 + 6,
                    ("A", "Z") => 3 + 0,
                    ("B", "X") => 1 + 0,
                    ("B", "Y") => 2 + 3,
                    ("B", "Z") => 3 + 6,
                    ("C", "X") => 1 + 6,
                    ("C", "Y") => 2 + 0,
                    ("C", "Z") => 3 + 3,
                    _ => panic!("erorr")
                };

                sum += points;
            }
        }

        Some(sum.to_string())
    }

    fn part2(&self) -> Option<String> {
        let input = read_input(self.args());
        let lines: Vec<&str> = input.lines().collect();

        let mut sum = 0;

        for line in lines {
            if line.len() > 0 {
                let r: Vec<&str> = line.split(" ").collect();
                let round: (&str, &str) = (r[0], r[1]);

                let points = match round {
                    ("A", "X") => 3 + 0, // r s
                    ("A", "Y") => 1 + 3, // r r
                    ("A", "Z") => 2 + 6, // r p
                    ("B", "X") => 1 + 0, // p r
                    ("B", "Y") => 2 + 3, // p p
                    ("B", "Z") => 3 + 6, // p s
                    ("C", "X") => 2 + 0, // s p
                    ("C", "Y") => 3 + 3, // s s
                    ("C", "Z") => 1 + 6, // s r
                    _ => panic!("erorr")
                };

                sum += points;
            }
        }

        Some(sum.to_string())    }
}
