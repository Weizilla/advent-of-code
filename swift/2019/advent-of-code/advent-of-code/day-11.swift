import Foundation

private let directions: [Direction] = [.UP, .RIGHT, .DOWN, .LEFT]

func day11Part1() -> Int {
    let intCodes = readInputInt(11, separator: ",")
    print(intCodes)

    var currLoc = [0, 0]
    var whiteLocs = Set<[Int]>()
    var currDir = Direction.UP
    var painted = Set<[Int]>()

    let program = IntCode(intCodes)

    while (!program.hasHalt) {
        let input = whiteLocs.contains(currLoc) ? 1 : 0
//        print("input \(input) curr loc \(currLoc) curr dir \(currDir)")
        let outputs = program.run([input])
//        print("outputs \(outputs)")

        let newColor = outputs[0]
        if newColor == 0 {
            whiteLocs.remove(currLoc)
        } else if newColor == 1 {
            whiteLocs.insert(currLoc)
        }
        painted.insert(currLoc)

        currDir = turn(currDir: currDir, input: outputs[1])
        currLoc = move(currDir: currDir, currLoc: currLoc)
    }

    return painted.count
}

private func turn(currDir: Direction, input: Int) -> Direction {
    var command = input == 0 ? -1 : input
    var dirIndex = directions.firstIndex(of: currDir)!
    dirIndex += command
    if (dirIndex == -1) {
        dirIndex = 3
    } else {
        dirIndex = dirIndex % 4
    }
    return directions[dirIndex]
}

private func move(currDir: Direction, currLoc: [Int]) -> [Int] {
    switch (currDir) {
    case .UP: return [currLoc[0], currLoc[1] - 1]
    case .RIGHT:return [currLoc[0] + 1, currLoc[1]]
    case .LEFT:return [currLoc[0] - 1, currLoc[1]]
    case .DOWN:return [currLoc[0], currLoc[1] + 1]
    }
}

func day11Part2() -> Int {
    let intCodes = readInputInt(11, separator: ",")
    print(intCodes)

    var currLoc = [0, 0]
    var whiteLocs = Set<[Int]>()
    var currDir = Direction.UP
    var painted = Set<[Int]>()

    let program = IntCode(intCodes)

    whiteLocs.insert(currLoc)

    while (!program.hasHalt) {
        let input = whiteLocs.contains(currLoc) ? 1 : 0
//        print("input \(input) curr loc \(currLoc) curr dir \(currDir)")
        let outputs = program.run([input])
//        print("outputs \(outputs)")

        let newColor = outputs[0]
        if newColor == 0 {
            whiteLocs.remove(currLoc)
        } else if newColor == 1 {
            whiteLocs.insert(currLoc)
        }
        painted.insert(currLoc)

        currDir = turn(currDir: currDir, input: outputs[1])
        currLoc = move(currDir: currDir, currLoc: currLoc)
    }

    let minX = whiteLocs.map({$0[0]}).min()!
    let maxX = whiteLocs.map({$0[0]}).max()!
    let minY = whiteLocs.map({$0[1]}).min()!
    let maxY = whiteLocs.map({$0[1]}).max()!

    var result = ""
    for y in minY...maxY {
        for x in minX...maxX {
            let panel = whiteLocs.contains([x, y]) ? "#" : " "
            result += panel
        }
        result += "\n"
    }
    print(result)

    return 0
}

private enum Direction: String {
    case UP, DOWN, LEFT, RIGHT
}
