import Foundation

func day24Part1() -> Int {
    let input = readInput(24)
    let allDirections = input.map(parseLine)
    print(allDirections)

    var blackTiles: [[Int]: Bool] = [:]
    for directions in allDirections {
        let finalLoc = traverse(directions)
        blackTiles.merge([finalLoc: true], uniquingKeysWith: {(a, b) in !b} )
    }

    print(blackTiles)

    return blackTiles.values.filter({$0}).count
}

func day24Part2() -> Int {
    let input = readInput(24)
    let allDirections = input.map(parseLine)
    print(allDirections)

    var blackTiles: [[Int]: Bool] = [:]
    for directions in allDirections {
        let finalLoc = traverse(directions)
        blackTiles.merge([finalLoc: true], uniquingKeysWith: {(a, _) in !a} )
    }

    print(blackTiles)
    print("num \(numBlack(&blackTiles))")

    for i in 0..<100 {
        addSurrounding(&blackTiles)
        var newBlackTiles: [[Int]: Bool] = [:]
        for (c, isBlack) in blackTiles {
            let numAdj = numAdjBlack(c, &blackTiles)
            if (isBlack) {
                if numAdj == 1 || numAdj == 2 {
                    newBlackTiles[c] = true
                }
            } else {
                if numAdj == 2 {
                    newBlackTiles[c] = true
                }
            }
        }
        blackTiles = newBlackTiles.filter({(c, b) in b})
//        print("num [\(i)] \(numBlack(&blackTiles))")
    }


    return numBlack(&blackTiles)
}

func parseLine(_ line: String) -> [Direction] {
    var directions: [Direction] = []

    let l = Array(line)
    var i = 0
    while (i < line.count) {
        let c1 = l[i]
        switch c1 {
        case "w": directions.append(.W)
        case "e": directions.append(.E)
        case "n":
            i += 1
            let c2 = l[i]
            switch c2 {
            case "w": directions.append(.NW)
            case "e": directions.append(.NE)
            default: fatalError("Unknown \(c1)")
            }
        case "s":
            i += 1
            let c2 = l[i]
            switch c2 {
            case "w": directions.append(.SW)
            case "e": directions.append(.SE)
            default: fatalError("Unknown \(c1)")
            }
        default: fatalError("Unknown \(c1)")
        }
        i += 1
    }

    return directions
}

func traverse(_ directions: [Direction]) -> [Int] {
    var x = 0
    var y = 0

    for direction in directions {
        x += direction.deltaX
        y += direction.deltaY
    }

    return [x, y]
}

func addSurrounding(_ blackTiles: inout[[Int]: Bool]) {
    for coordinate in blackTiles.keys {
        for direction in Direction.allCases {
            let newCoor = [coordinate[0] + direction.deltaX, coordinate[1] + direction.deltaY]
            if !blackTiles.keys.contains(newCoor) {
                blackTiles[newCoor] = false
            }
        }
    }
}

func numAdjBlack(_ coordinate: [Int], _ blackTiles: inout [[Int]: Bool]) -> Int {
    var num = 0
    for direction in Direction.allCases {
        let newCoor = [coordinate[0] + direction.deltaX, coordinate[1] + direction.deltaY]
        if let isBlack = blackTiles[newCoor] {
            if isBlack {
                num += 1
            }
        }
    }
    return num
}

func numBlack(_ blackTiles: inout [[Int]: Bool]) -> Int {
    blackTiles.values.filter({$0}).count
}

enum Direction: String, CustomStringConvertible, CaseIterable {
    case SE
    case SW
    case E
    case W
    case NE
    case NW

    var deltaX: Int {
        switch self {
        case .SE: return 1
        case .SW: return -1
        case .E: return 2
        case .W: return -2
        case .NE: return 1
        case .NW: return -1
        }
    }

    var deltaY: Int {
        switch self {
        case .SE: return -1
        case .SW: return -1
        case .E: return 0
        case .W: return 0
        case .NE: return 1
        case .NW: return 1
        }
    }
    var description: String {
        self.rawValue
    }
}
