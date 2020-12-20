import Foundation

func day17() -> Int {
    let input = readInput(17)
    var grid = Grid()
    for y in 0..<input.count {
        for x in 0..<input[0].count {
            grid.set(x, y, 0, 0, Array(input[y])[x])
        }
    }
    print(grid)

    for i in 0..<6 {
        var newGrid = Grid()
//        print(" \(i) ----------")
        for w in grid.wBounds[0]-1...grid.wBounds[1]+1 {
            for z in grid.zBounds[0] - 1...grid.zBounds[1] + 1 {
                for y in grid.yBounds[0] - 1...grid.yBounds[1] + 1 {
                    for x in grid.xBounds[0] - 1...grid.xBounds[1] + 1 {
                        let numA = numActiveNeighbors(grid: &grid, point: [x, y, z, w])
                        //  print("\([x, y, z]) \(numA)")
                        if (grid.get(x, y, z, w) == "#") {
                            if (numA == 2 || numA == 3) {
                                newGrid.set(x, y, z, w, "#")
                            } else {
                                newGrid.set(x, y, z, w, ".")
                            }
                        } else if (grid.get(x, y, z, w) == ".") {
                            if (numA == 3) {
                                newGrid.set(x, y, z, w, "#")
                            } else {
                                newGrid.set(x, y, z, w, ".")
                            }
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
                for w in -1...1 {
                    if (!(x == 0 && y == 0 && z == 0 && w == 0)
                        && grid.get(x + point[0], y + point[1], z + point[2], w + point[3]) == "#") {
                        num += 1
                    }
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
    var wBounds = [0, 0]
    private var _numActive = 0

    func set(_ x: Int, _ y: Int, _ z: Int, _ w: Int, _ value: Character) {
        let key = [x, y, z, w]
        grid[key] = value

        _numActive += value == "#" ? 1 : 0

        xBounds = [min(x, xBounds[0]), max(x, xBounds[1])]
        yBounds = [min(y, yBounds[0]), max(y, yBounds[1])]
        zBounds = [min(z, zBounds[0]), max(z, zBounds[1])]
        wBounds = [min(w, wBounds[0]), max(w, wBounds[1])]
    }

    func get(_ x: Int, _ y: Int, _ z: Int, _ w: Int) -> Character {
        let key = [x, y, z, w]
        return grid[key] ?? "."
    }

    var description: String {
        var output = ""
        for z in zBounds[0]...zBounds[1] {
            output += "z=\(z)\n"
            for y in yBounds[0]...yBounds[1] {
                for x in xBounds[0]...xBounds[1] {
                    output += String(get(x, y, z, 0))
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
