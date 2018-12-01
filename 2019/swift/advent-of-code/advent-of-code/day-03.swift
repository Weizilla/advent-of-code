import Foundation

func day03Part1() -> Int {
    let input = readInput(3)
    var path1 = readSteps(input[0])
    var path2 = readSteps(input[1])

    let locations1 = travel1(&path1)
    let locations2 = travel1(&path2)

//    print("1: \(path1) \(locations1)")
//    print("2: \(path2) \(locations2)")

    let diff = locations1.intersection(locations2).sorted(by: {$0[0] < $1[0]})
    print("\(diff)")

    return diff[0][0]
}

func day03Part2() -> Int {
    let input = readInput(3)
    var path1 = readSteps(input[0])
    var path2 = readSteps(input[1])

    let (locations1, distance1) = travel2(&path1)
    let (locations2, distance2) = travel2(&path2)

//    print("1: \(path1) \(locations1) \(distance1)")
//    print("2: \(path2) \(locations2) \(distance2)")

    let diffs = locations1.intersection(locations2)
//    print("diff \(diffs)")

    var totalDists = [Int]()
    for diff in diffs {
        let totalDist = distance1[diff]! + distance2[diff]!
        totalDists.append(totalDist)
    }

    print("total \(totalDists)")

    return totalDists.min()!
}

private func readSteps(_ input: String) -> [Step] {
    var steps = [Step]()

    for part in input.components(separatedBy: ",") {
        let direction = Direction(rawValue: String(Array(part)[0]))!
        let value = Int(String(part.dropFirst()))!
        steps.append(Step(direction: direction, value: value))
    }

    return steps
}

private func travel1(_ steps: inout [Step]) -> Set<[Int]> {
    var results = Set<[Int]>()

    // ^
    // |
    // x y ->

    var currX = 0
    var currY = 0

    for step in steps {
        switch (step.direction) {
        case .U:
            for i in 1...step.value {
                currX += 1
                results.insert([abs(currX) + abs(currY), currX, currY])
            }
        case .D:
            for i in 1...step.value {
                currX -= 1
                results.insert([abs(currX) + abs(currY), currX, currY])
            }
        case .L:
            for i in 1...step.value {
                currY -= 1
                results.insert([abs(currX) + abs(currY), currX, currY])
            }
        case .R:
            for i in 1...step.value {
                currY += 1
                results.insert([abs(currX) + abs(currY), currX, currY])
            }
        }

    }

    return results
}

private func travel2(_ steps: inout [Step]) -> (Set<[Int]>, [[Int]: Int]) {
    var results = Set<[Int]>()
    var distances = [[Int]: Int]()

    // ^
    // |
    // x y ->

    var currX = 0
    var currY = 0

    var distance = 0
    for step in steps {
        switch (step.direction) {
        case .U:
            for i in 1...step.value {
                currX += 1
                results.insert([currX, currY])
                distance += 1
                distances.merge([[currX, currY]: distance], uniquingKeysWith: {(c, _) in c})
            }
        case .D:
            for i in 1...step.value {
                currX -= 1
                results.insert([currX, currY])
                distance += 1
                distances.merge([[currX, currY]: distance], uniquingKeysWith: {(c, _) in c})
            }
        case .L:
            for i in 1...step.value {
                currY -= 1
                results.insert([currX, currY])
                distance += 1
                distances.merge([[currX, currY]: distance], uniquingKeysWith: {(c, _) in c})
            }
        case .R:
            for i in 1...step.value {
                currY += 1
                results.insert([currX, currY])

                distance += 1
                distances.merge([[currX, currY]: distance], uniquingKeysWith: {(c, _) in c})
            }
        }

    }

    return (results, distances)
}


private struct Step: CustomStringConvertible {
    let direction: Direction
    let value: Int

    var description: String {
        "\(direction.rawValue)\(value)"
    }
}

private enum Direction: String {
    case U, D, L, R
}
