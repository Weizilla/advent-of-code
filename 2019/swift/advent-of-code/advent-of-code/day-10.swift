import Foundation

func day10Part1() -> Int {
    let input = readInput(10)
    var asteroids = readAsteroids(input)
    return calcMaxVis(&asteroids).1
}

func day10Part2() -> Int {
    let input = readInput(10)
    var asteroids = readAsteroids(input)
    let maxVis = calcMaxVis(&asteroids)
    print("max \(maxVis)")

    let center = maxVis.0
    var coors = [QuadSlope: [Coordinates]]()
    for check in asteroids {
        if center == check {
            continue
        }
        let slopeX = check[0] - center[0]
        let slopeY = check[1] - center[1]
        let coor = Coordinates(x: check[0], y: check[1], dx: slopeX, dy: slopeY)
        let quadSlope = QuadSlope(dx: slopeX, dy: slopeY)
        coors.merge([quadSlope: [coor]], uniquingKeysWith: +)
    }

    for (k, v) in coors {
        coors[k] = v.sorted()
    }

    let keys = Array(coors.keys.sorted())
    var i = 0
    var hits = [Coordinates]()

    while (coors.values.map({$0.count}).reduce(0, +) > 0) {
        let key = keys[i % keys.count]
        var coorForKey = coors[key]!
        if !coorForKey.isEmpty {
            let firstCoor = coorForKey.removeFirst()
            coors[key] = coorForKey
            hits.append(firstCoor)
        }
        i += 1
    }

    return hits[199].x * 100 + hits[199].y
}

private func calcMaxVis(_ asteroids: inout [[Int]]) -> ([Int], Int) {
    var numVisible = [[Int]: Int]()
    for var curr in asteroids {
        var slopes = [
            [false, false]: Set<String>(),
            [false, true]: Set<String>(),
            [true, false]: Set<String>(),
            [true, true]: Set<String>(),
        ]
        for check in asteroids {
            if curr == check {
                continue
            }
            let slopeX = check[0] - curr[0]
            let slopeY = check[1] - curr[1]
            let slopeStr = slopeAsString(slopeX, slopeY)
            slopes.merge([[slopeX >= 0, slopeY >= 0]: [slopeStr]], uniquingKeysWith: {$0.union($1)})
        }
        numVisible[curr] = slopes.values.map({$0.count}).reduce(0, +)
    }

    guard let maxVis = numVisible.max(by:{$0.value < $1.value}) else { fatalError()}
    return maxVis
}

private func slopeAsString(_ dx: Int, _ dy: Int) -> String {
    String(round(1_000 * Double(dy) / Double(dx)) / 1_000)
}

private func readAsteroids(_ inputs: [String]) -> [[Int]] {
    var asteroids = [[Int]]()
    for (y, line) in inputs.enumerated() {
        for (x, char) in Array(line).enumerated() {
            if char == "#" {
                asteroids.append([x, y])
            }
        }
    }
    return asteroids
}

private class Coordinates: Comparable, CustomStringConvertible {
    let x: Int
    let y: Int
    let dist: Int
    var rowNum: Int

    init(x: Int, y: Int, dx: Int, dy: Int) {
        self.x = x
        self.y = y
        self.rowNum = 0
        self.dist = abs(dx) + abs(dy)
    }

    static func <(lhs: Coordinates, rhs: Coordinates) -> Bool {
        return lhs.dist < rhs.dist
    }

    static func ==(lhs: Coordinates, rhs: Coordinates) -> Bool {
        return lhs.dist == rhs.dist
    }

    var description: String {
        "x=\(x) y=\(y) dist=\(dist)"
    }
}

private class QuadSlope: Comparable, Equatable, Hashable, CustomStringConvertible {
    let quadrant: Quadrant
    let dx: Int
    let dy: Int

    init(dx: Int, dy: Int) {
        self.dx = dx
        self.dy = dy

        if dx >= 0 && dy >= 0 {
            self.quadrant = .SE
        } else if dx >= 0 && dy < 0 {
            self.quadrant = .NE
        } else if dx < 0 && dy >= 0 {
            self.quadrant = .SW
        } else if dx < 0 && dy < 0 {
            self.quadrant = .NW
        } else {
            fatalError("no quad")
        }
    }

    var slope: String {
        slopeAsString(dx, dy)
    }

    var slopeNum: Double {
        Double(slope)!
    }

    var dist: Int {
        abs(dx) + abs(dy)
    }

    static func <(lhs: QuadSlope, rhs: QuadSlope) -> Bool {
        if lhs.quadrant != rhs.quadrant {
            return lhs.quadrant < rhs.quadrant
        }
        if lhs.slope != rhs.slope {
            switch (lhs.quadrant) {
            case .NE: return lhs.slopeNum < rhs.slopeNum
            case .SE: return lhs.slopeNum < rhs.slopeNum
            case .SW: return lhs.slopeNum < rhs.slopeNum
            case .NW: return lhs.slopeNum < rhs.slopeNum
            }
        }
        return false
    }

    static func ==(lhs: QuadSlope, rhs: QuadSlope) -> Bool {
        lhs.quadrant == rhs.quadrant && lhs.slope == rhs.slope
    }

    var description: String {
        "\(quadrant) dx=\(dx) dy=\(dy) slope=\(slope)"
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(quadrant)
        hasher.combine(slope)
    }
}

private enum Quadrant: String, Comparable, Hashable {
    case NE, SE, SW, NW

    private var sortOrder: Int {
        switch self {
        case .NE: return 0
        case .SE: return 1
        case .SW: return 2
        case .NW: return 3
        }
    }

    static func <(lhs: Quadrant, rhs: Quadrant) -> Bool {
        lhs.sortOrder < rhs.sortOrder
    }

    static func ==(lhs: Quadrant, rhs: Quadrant) -> Bool {
        lhs.sortOrder == rhs.sortOrder
    }
}
