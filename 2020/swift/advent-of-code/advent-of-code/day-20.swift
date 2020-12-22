import Foundation

func day20Part1() -> Int {
    var lines = readInput(20)

    var tiles: [Tile] = []
    var currTile: Tile? = nil
    var allBorders: [[Character]: [Int]] = [:]

    for line in lines {
        if line.contains("Tile") {
            currTile = Tile(parseId(line))
            tiles.append(currTile!)
        } else {
            currTile!.addRow(line)
        }
    }

    for tile in tiles {
        tile.allBorders.forEach({allBorders.merge([$0: [tile.id]], uniquingKeysWith: +)})
    }

//    for (b, ids) in allBorders {
//        print("\(String(b)) \(ids))")
//    }

    var numBordersPerTile: [Int: Int] = [:]
    for tile in tiles {
        var num = 0
        for border in tile.allBorders {
            num += allBorders[border]!.count
        }
        numBordersPerTile[tile.id] = num
    }

//    print(numAllBorderPerTile)

    return numBordersPerTile.filter({$1 == 12}).map({$0.key}).reduce(1, *)
}

func parseId(_ line: String) -> Int {
    Int(line.split(separator: " ")[1].replacingOccurrences(of: ":", with: ""))!
}

class Tile: CustomStringConvertible {
    let id: Int
    var tile: [[Character]]

    init(_ id: Int) {
        self.id = id
        tile = []
    }

    var borders: [[Character]] {
        var b: [[Character]] = []
        b.append(tile[0])
        b.append(tile[tile.count - 1])
        b.append(tile.map({$0[0]}))
        b.append(tile.map({$0[tile[0].count - 1]}))
        return b
    }

    var allBorders: [[Character]] {
        borders.flatMap({[$0, $0.reversed()]})
    }

    func addRow(_ row: String) {
        tile.append(Array(row))
    }

    var description: String {
        var line = "\(id)\n"
        for t in tile {
            line += "\(String(t))\n"
        }
        return line
    }
}
