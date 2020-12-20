import Foundation

func day17Part1() -> Int {
    let input = readInput(17)
    // z, y, x
    var grid = Grid()
    for y in 0..<input.count {
        for x in 0..<input[0].count {
            grid.set(x, y, 0, Array(input[y])[x])
        }
    }
    print(grid)

    for i in 0..<6 {
        var newGrid = Grid()
        print(" \(i) ----------")
        for z in grid.zBounds[0]-1...grid.zBounds[1]+1 {
            for y in grid.yBounds[0]-1...grid.yBounds[1]+1 {
                for x in grid.xBounds[0]-1...grid.xBounds[1]+1 {
                    let numA = numActiveNeighbors(grid: &grid, point: [x, y, z])
//                    print("\([x, y, z]) \(numA)")
                    if (grid.get(x, y, z) == "#") {
                        if (numA == 2 || numA == 3) {
                            newGrid.set(x, y, z, "#")
                        } else {
                            newGrid.set(x, y, z, ".")
                        }
                    } else if (grid.get(x, y, z) == ".") {
                        if (numA == 3) {
                            newGrid.set(x, y, z, "#")
                        } else {
                            newGrid.set(x, y, z, ".")
                        }
                    }
                }
            }
        }
//        print(newGrid)
        grid = newGrid
    }

    return grid.numActive
}

func numActiveNeighbors(grid: inout Grid, point: [Int]) -> Int {
    var num = 0
    for x in -1...1 {
        for y in -1...1 {
            for z in -1...1 {
                if (!(x == 0 && y == 0 && z == 0) && grid.get(x + point[0], y + point[1], z + point[2]) == "#") {
                    num += 1
                }
            }
        }
    }
    return num
}

class Grid: CustomStringConvertible {
    var grid: [[Int]: Character] = [:]
    var xBounds = [0, 0]
    var yBounds = [0, 0]
    var zBounds = [0, 0]
    private var _numActive = 0

    func set(_ x: Int, _ y: Int, _ z: Int, _ value: Character) {
        let key = [x, y, z]
        grid[key] = value

        _numActive += value == "#" ? 1 : 0

        xBounds = [min(x, xBounds[0]), max(x, xBounds[1])]
        yBounds = [min(y, yBounds[0]), max(y, yBounds[1])]
        zBounds = [min(z, zBounds[0]), max(z, zBounds[1])]
    }

    func get(_ x: Int, _ y: Int, _ z: Int) -> Character {
        let key = [x, y, z]
        return grid[key] ?? "."
    }

    var description: String {
        var output = ""
        for z in zBounds[0]...zBounds[1] {
            output += "z=\(z)\n"
            for y in yBounds[0]...yBounds[1] {
                for x in xBounds[0]...xBounds[1] {
                    output += String(get(x, y, z))
                }
                output += "\n"
            }
            output += "\n"
        }
        return output
    }

    var numActive: Int {
        _numActive
    }
}
