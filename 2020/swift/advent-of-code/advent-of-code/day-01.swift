import Foundation

let test1Input = [
    1721,
    979,
    366,
    299,
    675,
    1456,
]

let part1Expected = 514579
let part2Expected = 241861950

func day1Part1() -> Int {
    let inputs: Set = Set(readInputInt(1))
    for input in inputs {
        let other = 2020 - input
        if inputs.contains(other) {
            return input * other
        }
    }
    fatalError("Not found")
}

func day1Part2() -> Int {
    let inputs: Set = Set(readInputInt(1))
    for input1 in inputs {
        for input2 in inputs {
            let remaining = 2020 - input1 - input2
            if inputs.contains(remaining) {
                return input1 * input2 * remaining
            }
        }
    }
    fatalError("Not found")
}
