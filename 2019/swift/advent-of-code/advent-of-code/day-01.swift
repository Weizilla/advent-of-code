import Foundation

func day01Part1() -> Int {
    let input = readInputInt(1)
    return input
        .map {calcFuel1($0)}
        .reduce(0, +)
}

func day01Part2() -> Int {
    let input = readInputInt(1)
    return input
        .map {calcFuel2($0)}
        .reduce(0, +)
}

func calcFuel1(_ input: Int) -> Int {
    if input < 0 {
        return 0
    }
    let fuel = Int(floor(Double(input) / 3.0)) - 2
    return fuel
}

func calcFuel2(_ input: Int) -> Int {
    if input < 0 {
        return 0
    }
    let fuel = Int(floor(Double(input) / 3.0)) - 2
    return fuel < 0 ? 0 : fuel + calcFuel2(fuel)
}
