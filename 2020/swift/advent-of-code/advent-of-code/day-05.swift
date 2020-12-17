import Foundation

func day05Part1() -> Int {
    let inputs = readInput(5)
    return inputs.map({toSeatId($0)}).max()!
}

func day05Part2() -> Int {
    let inputs = readInput(5)
    let ids = inputs.map({toSeatId($0)}).sorted()
    print(ids)
    let diff = ids[0]
    for i in 0..<ids.count {
        if ids[i] - diff != i {
            return ids[i] - 1
        }
    }
    return 0
}

func toSeatId(_ input: String) -> Int {
    let binInput = input
        .replacingOccurrences(of: "F", with: "0")
        .replacingOccurrences(of: "B", with: "1")
        .replacingOccurrences(of: "L", with: "0")
        .replacingOccurrences(of: "R", with: "1")
    return Int(binInput, radix: 2)!
}
