import Foundation

func day10Part1() -> Int {
    var input = readInputInt(10).sorted()
    input.insert(0, at: 0)
    input.insert(input.last! + 3, at: input.count)

    var diffCounts: [Int:Int] = [:]

    for i in 1..<input.count {
        let diff = input[i] - input[i - 1]
        diffCounts.merge([diff: 1], uniquingKeysWith: +)
        print(diffCounts)
    }
    return diffCounts[1]! * diffCounts[3]!
}
