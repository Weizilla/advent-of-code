use std::fs;

// This is prob bad shoving OOP into rust
// Revisit https://doc.rust-lang.org/stable/book/ch17-00-oop.html
pub trait Solution {
    fn year(&self) -> i32;
    fn day(&self) -> i32;
    fn part1(&self) -> Option<String>;
    fn part2(&self) -> Option<String>;
}

pub fn read_input(year: i32, day: i32) -> String {
    let day = format!("{:02}", day);
    let file_path = format!("../../../inputs/{year}/day-{day}-input.txt");
    let contents = fs::read_to_string(file_path).expect("Error reading input");
    return contents;
}
