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

func day10Part2() -> Int {
    var input = readInputInt(10, example: 2).sorted()
    input.insert(0, at: 0)
    input.insert(input.last! + 3, at: input.count)

    var allInputs = Set(input)

    // [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]
    // paths
    //  1  1  3  2  1  1  2   1   1   1   1   1   1

    // 4 -> 5 (don't count)
    // 4 -> 6
    // 4 -> 7

    // 5 -> 6
    // 5 -> 7

    // 10 -> 11
    // 10 -> 12

    // 2 * (1 * 2) * 2

    var pathCount: [Int: Int] = [:]

    for i in 0..<input.count {
        let curr = input[input.count - i - 1]
        let count = (1..<4).map({curr + $0})
            .filter({allInputs.contains($0)})
            .filter({pathCount[$0] == nil})
            .count
        if (count > 1) {
            pathCount[curr] = count
        }
    }

    print(pathCount)

    return pathCount.values.reduce(1, *)
}
