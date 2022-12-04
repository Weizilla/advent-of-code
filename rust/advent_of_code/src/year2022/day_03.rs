use std::collections::HashSet;

use advent_of_code::{InputType, read_input, SolutionArgs};
use advent_of_code::Solution;

pub struct Day03 {}

impl Solution for Day03 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 3,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let input = read_input(self.args());
        let lines: Vec<&str> = input.lines().collect();
        let mut sum = 0;
        for line in lines {
            let dup = find_both(line);
            let value = calc_value(dup);
            println!("{} {}", dup, value);
            sum += value;
        }
        Some(sum.to_string())
    }

    fn part2(&self) -> Option<String> {
        let input = read_input(self.args());
        let lines: Vec<&str> = input.lines().collect();
        let chunks: Vec<_> = lines.chunks(3).collect();

        let mut sum = 0;
        for chunk in chunks {
            let dup = find_dups(chunk);
            let value = calc_value(dup);
            sum += value;
        }

        Some(sum.to_string())
    }
}


fn find_both(sacks: &str) -> char {
    let half_index = sacks.len() / 2;
    let first_half = &sacks[0..half_index];
    let second_half = &sacks[half_index..];

    let first_half_uniq = string_to_char_set(first_half);

    let second_chars = second_half.chars().collect::<Vec<char>>();
    let second_half_uniq: HashSet<char> = HashSet::from_iter(second_chars);

    // println!("{:?}", &first_half_uniq);
    // println!("{:?}", &second_half_uniq);
    let dups: Vec<&char> = first_half_uniq.intersection(&second_half_uniq).collect();

    // println!("dup {:?}", dups);
    return *dups[0];
}

fn string_to_char_set(first_half: &str) -> HashSet<char> {
    let chars = first_half.chars().collect::<Vec<char>>();
    HashSet::from_iter(chars)
}

fn find_dups(strings: &[&str]) -> char {
    let mut a_chars = string_to_char_set(strings[0]);
    let b_chars = string_to_char_set(strings[1]);
    let c_chars = string_to_char_set(strings[2]);

    a_chars.retain(|x| b_chars.contains(x));
    a_chars.retain(|x| c_chars.contains(x));

    let v: Vec<&char> = a_chars.iter().collect();

    return *v[0];
}

fn calc_value(input: char) -> u32 {
    let i = input as u32;
    let a = if input.is_uppercase() { 'A' } else { 'a' } as u32;
    let u = if input.is_uppercase() { 26 } else { 0 };
    i - a + u + 1
}
