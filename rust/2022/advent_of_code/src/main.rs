use advent_of_code::Solution;

mod year2022;

fn main() {
    let solution = year2022::day_01::Day01 {};
    if let Some(part2) = solution.part2() {
        println!("Year {} day {} part 2 result {}", solution.year(), solution.day(), part2);
    } else if let Some(part1) = solution.part1() {
        println!("Year {} day {} part 1 result {}", solution.year(), solution.day(), part1);
    } else {
        println!("Year {} day {} not done", solution.year(), solution.day());
    }
}
