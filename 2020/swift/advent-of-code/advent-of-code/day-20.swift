import Foundation

func day20Part1() -> Int {
    var input = readInput(20)
    let tiles = readTiles1(input)
    var allBorders: [[Character]: [Int]] = [:]

    for tile in tiles {
        tile.borders.forEach({allBorders.merge([$0: [tile.id]], uniquingKeysWith: +)})
        tile.borders.forEach({allBorders.merge([$0.reversed(): [tile.id]], uniquingKeysWith: +)})
    }
//
//    for (b, ids) in allBorders {
//        print("\(b) \(ids))")
//    }

    var numBordersPerTile: [Int: Int] = [:]
    for tile in tiles {
        var num = 0
        for border in tile.borders {
            num += allBorders[border]!.count
        }
        numBordersPerTile[tile.id] = num
    }

    print(numBordersPerTile)

    return numBordersPerTile.filter({$1 == 6}).map({$0.key}).reduce(1, *)
}

func readTiles1(_ input: [String]) -> [Tile1] {
    var tiles: [Tile1] = []
    var currTileId: Int? = nil
    var currTile: [[Character]] = []

    for line in input {
        if line.contains("Tile") {
            if currTileId != nil {
                tiles.append(Tile1(currTileId!, currTile))
            }
            currTileId = parseIdDay20(line)
            currTile = []
        } else {
            currTile.append(Array(line))
        }
    }
    if currTileId != nil {
        tiles.append(Tile1(currTileId!, currTile))
    }
    return tiles
}

func parseIdDay20(_ line: String) -> Int {
    Int(line.split(separator: " ")[1].replacingOccurrences(of: ":", with: ""))!
}

class Tile1: CustomStringConvertible {
    let id: Int
    var tile: [[Character]]
    var borders: [[Character]]

    init(_ id: Int, _ tile: [[Character]]) {
        self.id = id
        self.tile = tile

        let top = tile[0]
        let bottom = tile[tile.count - 1]
        let left = tile.map({$0[0]})
        let right = tile.map({$0[tile[0].count - 1]})

        borders = [top, bottom, left, right]
    }

    var description: String {
        var line = "[\(id)]"
//        for t in tile {
//            line += "\(String(t))\n"
//        }
        return line
    }
}
