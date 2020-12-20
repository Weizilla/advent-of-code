import Foundation

func day17Part1() -> Int {
    let input = readInput(17, example: 1)
    // z, y, x
    var grid = Table()
    for y in 0..<input.count {
        for x in 0..<input[0].count {
            grid.set(x, y, 0, Array(input[y])[x])
        }
    }
    print(grid)
    return 0
}

class Table: CustomStringConvertible {
    var table: [Int: [Int: [Int: Character]]] = [:]
    var xBounds = [0, 0]
    var yBounds = [0, 0]
    var zBounds = [0, 0]

    func set(_ x: Int, _ y: Int, _ z: Int, _ value: Character) {
        if var xt = table[x] {
            if var yt = xt[y] {
                yt[z] = value
            } else {
                xt[y] = [z: value]
                table[x] = xt
            }
        } else {
            table[x] = [y:[z: value]]
        }

        xBounds = [min(x, xBounds[0]), max(x, xBounds[1])]
        yBounds = [min(y, yBounds[0]), max(y, yBounds[1])]
        zBounds = [min(z, zBounds[0]), max(z, zBounds[1])]
    }

    func get(_ x: Int, _ y: Int, _ z: Int) -> Character {
        table[x]?[y]?[z] ?? " "
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
}
