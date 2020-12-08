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

func day01() -> Int {
//    part1()
    return part2()
}

func part1() -> Int {
    let inputs: Set = Set(readIntInput(1))
    for input in inputs {
        let other = 2020 - input
        if inputs.contains(other) {
            return input * other
        }
    }
    fatalError("Not found")
}

func part2() -> Int {
    let inputs: Set = Set(readIntInput(1))
    for input in inputs {
        let other = 2020 - input
        if inputs.contains(other) {
            return input * other
        }
    }
    fatalError("Not found")
}