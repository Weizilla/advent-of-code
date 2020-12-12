import Foundation

func day03Part1() -> Int {
    let slopeRight = 3
    let slopeDown = 1
    let input = readInput(3)
    var count = countTrees(input: input, slopeRight: slopeRight, slopeDown: slopeDown)
    return count
}

func day03Part2() -> Int {
    var input = readInput(3)
    var slopes = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2),
    ]
    var total = 1
    for slope in slopes {
        var count = countTrees(input: input, slopeRight: slope.0, slopeDown: slope.1)
        total *= count
    }
    return total
}

private func countTrees(input: [String], slopeRight: Int, slopeDown: Int) -> Int {
    var count = 0
    var rightCount = 0
    for i in stride(from: 0, through: input.count - 1, by: slopeDown) {
        let row = input[i]
        let right = (rightCount * slopeRight) % input[i].count
//        print("\(row) \(right) \(i)")
        let c = row[row.index(row.startIndex, offsetBy: right)]
        if c == "#" {
            count += 1
        }
        rightCount += 1
    }
    return count
}
