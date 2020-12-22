import Foundation

func day20Part1() -> Int {
    var input = readInput(20)
    let tiles = readTiles(input)
    var allBorders: [Border: [String]] = [:]

    for tile in tiles {
        tile.borders.forEach({allBorders.merge([$0: [tile.id]], uniquingKeysWith: +)})
    }
//
//    for (b, ids) in allBorders {
//        print("\(b) \(ids))")
//    }

    var numBordersPerTile: [String: Int] = [:]
    for tile in tiles {
        var num = 0
        for border in tile.borders {
            num += allBorders[border]!.count
        }
        numBordersPerTile[tile.id] = num
    }

    print(numBordersPerTile)

    return numBordersPerTile.filter({$1 == 12}).map({$0.key}).reduce(1, *)
}

func readTiles(_ input: [String]) -> [Tile] {
    var tiles: [Tile] = []
    var currTileId: String? = nil
    var currTile: [[Character]] = []

    for line in input {
        if line.contains("Tile") {
            if currTileId != nil {
                tiles.append(Tile(currTileId!, currTile))
                tiles.append(Tile("\(currTileId!)R", currTile))
            }
            currTileId = parseId(line)
            currTile = []
        } else {
            currTile.append(Array(line))
        }
    }
    if currTileId != nil {
        tiles.append(Tile(currTileId!, currTile))
    }
    return tiles
}

func day20Part2() -> Int {
    var input = readInput(20, example: 1)
    let tiles = readTiles(input)
    var allBorders: [Border: [Tile]] = [:]

    for tile in tiles {
        tile.borders.forEach({allBorders.merge([$0: [tile]], uniquingKeysWith: +)})
    }

    for tile in tiles {
        var totalTiles = 0
        for border in tile.borders {
            let numTiles = allBorders[border]!.count
            totalTiles += numTiles
            switch (numTiles) {
            case 1: border.type = .OUTSIDE
            case 2: border.type = .INSIDE
            default: fatalError("unexpected num: \(numTiles)")
            }
        }
        switch (totalTiles) {
        case 6: tile.type = .CORNER
        case 7: tile.type = .EDGE
        case 8: tile.type = .MIDDLE
        default: fatalError("unexpected num: \(totalTiles)")
        }
    }

    for tile in tiles {
        print(tile)
    }

    var width = Int(sqrt(Double(tiles.count)))

    var unplacedTiles = Set(tiles.map({$0.id}))
    var unplacedBorders = Set(allBorders.keys)
    var placedTiles: [[Int]: String] = [:]

    let firstCorner = tiles.filter({$0.type == .CORNER}).sorted(by: {$0.id < $1.id}).first!
//    print("first \(firstCorner)")
//    unplacedTiles.remove(firstCorner.id)
//    placedTiles[[0, 0]] = firstCorner.id
//    print(placedTiles)
//
//    let firstInside = firstCorner.borders.filter({allBorders[$0]!.count == 2}).first!
//    print("first edge: \(firstInside)")
//
//    let nextTile = allBorders[firstInside]!.filter({unplacedTiles.contains($0.id)}).first!
//    print("next tile \(nextTile)")
//    unplacedTiles.remove(nextTile.id)
//    placedTiles[[1, 0]] = nextTile.id
//    print(placedTiles)
//
//    let nextEdge = nextTile.oppositeBorders[firstInside]!
//    let next2Tile = allBorders[nextEdge]!.filter({unplacedTiles.contains($0.id)}).first!
//    print("next2 tile \(next2Tile)")
//    unplacedTiles.remove(next2Tile.id)
//    placedTiles[[2, 0]] = next2Tile.id
//    print(placedTiles)

    var prevTile = firstCorner
    var nextEdge = prevTile.borders.filter({ $0.type == .INSIDE}).first!
    unplacedTiles.remove(firstCorner.id)
    placedTiles[[0, 0]] = firstCorner.id
    for y in 0..<1 {
        for x in 0..<width {
            if (x == 0 && y == 0) {
                continue
            }

            let currTile = allBorders[nextEdge]!.filter({unplacedTiles.contains($0.id)}).first!
            unplacedTiles.remove(currTile.id)
            placedTiles[[x, y]] = currTile.id

            let n = currTile.oppositeBorders[nextEdge]!
            nextEdge = n
            prevTile = currTile
            print("pt \(placedTiles)")
        }
    }

    return 0

}


func parseId(_ line: String) -> String {
    line.split(separator: " ")[1].replacingOccurrences(of: ":", with: "")
}

class Tile: CustomStringConvertible {
    let id: String
    var tile: [[Character]]
    var type: TileType
    var borders: [Border]
    var oppositeBorders: [Border: Border]

    init(_ id: String, _ tile: [[Character]]) {
        self.id = id
        self.type = .UNKNOWN
        self.tile = tile
        oppositeBorders = [:]

        let top = Border(tile[0])
        let bottom = Border(tile[tile.count - 1])
        let left = Border(tile.map({$0[0]}))
        let right = Border(tile.map({$0[tile[0].count - 1]}))
        borders = [
            top,
            bottom,
            left,
            right
        ]
        oppositeBorders = [
            left: right,
            right: left,
            top: bottom,
            bottom: top,
        ]
    }

    var description: String {
        var line = "\(id) \(type) \n"
//        for t in tile {
//            line += "\(String(t))\n"
//        }
        return line
    }
}

class Border: Hashable, CustomStringConvertible {
    let value: [Character]
    var type: BorderType

    init(_ value: [Character]) {
        self.value = value
        type = .UNKNOWN
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(value)
    }
//
//    func reversed() -> Border {
//        Border(Array(value.reversed()))
//    }

    static func ==(lhs: Border, rhs: Border) -> Bool {
        lhs.value == rhs.value
    }

    var description: String {
        String(value)
    }
}

enum TileType {
    case CORNER
    case EDGE
    case MIDDLE
    case UNKNOWN
}

enum BorderType {
    case OUTSIDE
    case INSIDE
    case UNKNOWN
}
