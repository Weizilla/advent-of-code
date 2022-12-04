extern crate core;

use advent_of_code::Solution;

mod year2016;
mod year2022;

fn main() {
    let solution = year2022::day_04::Day04 {};
    let day = solution.args().day;
    let year = solution.args().year;

    if let Some(part2) = solution.part2() {
        println!("Year {} day {} part 2 result:\n\n{}", year, day, part2);
    } else if let Some(part1) = solution.part1() {
        println!("Year {} day {} part 1 result:\n\n{}", year, day, part1);
    } else {
        println!("Year {} day {} not done", year, day);
    }
}
