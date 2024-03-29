use advent_of_code::{InputType, read_lines, SolutionArgs};
use advent_of_code::Solution;

pub struct Day05 {}

impl Solution for Day05 {
    fn args(&self) -> SolutionArgs {
        SolutionArgs {
            year: 2022,
            day: 5,
            input_type: InputType::INPUT,
        }
    }

    fn part1(&self) -> Option<String> {
        let lines = read_lines(self.args());

        let mut setup: Vec<String> = Vec::new();
        let mut moves: Vec<String> = Vec::new();

        let mut is_setup = true;
        for line in lines {
            if line.len() == 0 {
                is_setup = false;
                continue;
            }
            if is_setup && line.contains("[") {
                setup.push(line);
            } else if line.contains("move") {
                moves.push(line);
            }
        }


        let mut s = parse_setup(&setup);
        let m = parse_moves(&moves);

        do_move(&mut s, &m);

        let answer = s.iter().map(|v| v.last().expect("error")).collect();

        Some(answer)
    }

    fn part2(&self) -> Option<String> {
        let lines = read_lines(self.args());

        let mut setup: Vec<String> = Vec::new();
        let mut moves: Vec<String> = Vec::new();

        let mut is_setup = true;
        for line in lines {
            if line.len() == 0 {
                is_setup = false;
                continue;
            }
            if is_setup && line.contains("[") {
                setup.push(line);
            } else if line.contains("move") {
                moves.push(line);
            }
        }


        let mut s = parse_setup(&setup);
        let m = parse_moves(&moves);

        do_move_2(&mut s, &m);

        let answer = s.iter().map(|v| v.last().expect("error")).collect();

        Some(answer)
    }
}

fn parse_setup(lines: &Vec<String>) -> Vec<Vec<char>> {
    let len = lines.len();
    let num_cols = (lines[len - 1].len() + 2) / 4;

    let mut all_stacks: Vec<Vec<char>> = Vec::new();

    for col in 0..num_cols {
        let mut stack: Vec<char> = Vec::new();

        for line in lines {
            let index = 1 + (col * 4);
            if index < line.len() {
                let char = line.chars().collect::<Vec<_>>()[index];
                if char != ' ' {
                    stack.push(char)
                }
            }
        }
        stack.reverse();
        all_stacks.push(stack);
    }


    all_stacks
}

fn parse_moves(lines: &Vec<String>) -> Vec<Move> {
    let mut moves: Vec<Move> = Vec::new();
    for line in lines {
        let parts: Vec<&str> = line.split(" ").collect();
        let num = parts[1].parse().expect("error");
        let from = parts[3].parse().expect("error");
        let to = parts[5].parse().expect("error");

        moves.push(Move { num, from, to })
    }

    return moves;
}

fn do_move(setup: &mut Vec<Vec<char>>, moves: &Vec<Move>) {
    for m in moves {
        for _ in 0..m.num {
            let f = &mut setup[m.from - 1];
            let c = f.pop().expect("error");
            let t = &mut setup[m.to - 1];
            t.push(c);
        }
    }
}

fn do_move_2(setup: &mut Vec<Vec<char>>, moves: &Vec<Move>) {
    // eprintln!("setup = {:?}", setup);
    for m in moves {
        // eprintln!("moves = {:?}", m);
        for i in 0..m.num {
            let f = &mut setup[m.from - 1];
            // eprintln!("i = {:?}", f.len() - (m.num - i) as usize);
            let c = f.remove(f.len() - (m.num - i) as usize);
            let t = &mut setup[m.to - 1];
            t.push(c);
        }
        // eprintln!("s = {:?}", setup);
    }
}

#[derive(Debug)]
struct Move {
    num: i32,
    from: usize,
    to: usize,
}
