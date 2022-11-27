use std::fs;

pub fn read_input(day: u32) -> String {
    let path: String = file!().to_string();
    println!("{}", path);
    let day = format!("{:02}", day);
    let file_path = format!("../../../inputs/2022/day-{day}-input.txt");
    let contents = fs::read_to_string(file_path).expect("Error reading input");
    return contents;
}
