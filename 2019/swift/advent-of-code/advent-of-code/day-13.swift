import Foundation

func day13Part1() -> Int {
    let intCodes = readInputInt(13, separator: ",")
    let program = IntCode(intCodes)
    let output = program.run([])
    print(output)

    var grid = [[Int]: Int]()

    for i in stride(from: 0, to: output.count, by: 3) {
        let x = output[i]
        let y = output[i + 1]
        let type = output[i + 2]
        grid[[x, y]] = type
    }

    return numBricks(&grid)
}

func day13Part2() -> Int {
    var intCodes = readInputInt(13, separator: ",")
    intCodes[0] = 2
    var program = IntCode(intCodes)
    var grid = [[Int]: Int]()

    var score = runStep(&program, 0, &grid)
    printGrid(&grid)

    while (numBricks(&grid) > 0) {
        let ballX = grid.filter({(_, v) in v == 4}).map({$0.key}).first![0]
        let paddleX = grid.filter({(_, v) in v == 3}).map({$0.key}).first![0]
        var input: Int
        if (ballX < paddleX) {
            input = -1
        } else if (ballX > paddleX) {
            input = 1
        } else {
            input = 0
        }

//        print("input \(input)")
        score = runStep(&program, input, &grid)
//        printGrid(&grid)
    }

    return score
}

private func runStep(_ program: inout IntCode, _ input: Int, _ grid: inout [[Int]: Int]) -> Int {
    let output = program.run([input])
    let (changed, score) = parseOutput(output)
    grid.merge(changed, uniquingKeysWith: {(_, new) in new})
    return score
}


private func numBricks(_ grid: inout [[Int]: Int]) -> Int {
    grid.values.filter({$0 == 2}).count
}

private func parseOutput(_ output: [Int]) -> ([[Int]: Int], Int) {
    var grid = [[Int]: Int]()
    var score = 0
    for i in stride(from: 0, to: output.count, by: 3) {
        let x = output[i]
        let y = output[i + 1]
        if x == -1 && y == 0 {
            score = output[i + 2]
        } else {
            let type = output[i + 2]
            grid[[x, y]] = type
        }
    }
    return (grid, score)
}

private func printGrid(_ grid: inout [[Int]: Int]) {
    let minX = grid.keys.map({$0[0]}).min()!
    let maxX = grid.keys.map({$0[0]}).max()!
    let minY = grid.keys.map({$0[1]}).min()!
    let maxY = grid.keys.map({$0[1]}).max()!

    var result = "--------------------\n"
    for y in minY..<maxY {
        for x in minX..<maxX {
            let type: Int = grid[[x, y]] ?? 0
            switch (type) {
            case 0: result += " "
            case 1: result += "#"
            case 2: result += "x"
            case 3: result += "="
            case 4: result += "O"
            default: fatalError("Unknown type \(type)")
            }
        }
        result += "\n"
    }
    print(result)
}

private enum BallDir: String {
    case LEFT
    case RIGHT
}
