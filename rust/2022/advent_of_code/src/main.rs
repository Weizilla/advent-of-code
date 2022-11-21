use std::fs;

mod day_01;


fn main() {
    let result = day_01::day01_part1();
    println!("{result}");
}

fn read_input(day: u32) -> String {
    let day = format!("{:02}", day);
    let file_path = format!("../../../inputs/2022/day-{day}-input.txt");
    let contents = fs::read_to_string(file_path).expect("Error reading input");
    return contents;
}
