import Foundation

func day20Part1() -> Int {
    var input = readInput(20)
    let tiles = readTiles(input)
    var allBorders: [Border: [Int]] = [:]

    for tile in tiles {
        tile.allBorders.keys.forEach({allBorders.merge([$0: [tile.id]], uniquingKeysWith: +)})
    }
//
//    for (b, ids) in allBorders {
//        print("\(b) \(ids))")
//    }

    var numBordersPerTile: [Int: Int] = [:]
    for tile in tiles {
        var num = 0
        for border in tile.allBorders.keys {
            num += allBorders[border]!.count
        }
        numBordersPerTile[tile.id] = num
    }

    print(numBordersPerTile)

    return numBordersPerTile.filter({$1 == 12}).map({$0.key}).reduce(1, *)
}

func readTiles(_ input: [String]) -> [Tile] {
    var tiles: [Tile] = []
    var currTileId: Int? = nil
    var currTile: [[Character]] = []

    for line in input {
        if line.contains("Tile") {
            if currTileId != nil {
                tiles.append(Tile(currTileId!, currTile))
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
    var width = Int(sqrt(Double(tiles.count)))

    // all borders for all orientations
    for tile in tiles {
        tile.allBorders.keys.forEach({allBorders.merge([$0: [tile]], uniquingKeysWith: +)})
    }

    // figure out border and tile types
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

//    for tile in tiles {
//        print(tile)
//    }

    // figure out tile id grid
    var placedTileIds: Set<Int> = []
    var placedBorderValues: Set<Border> = []
    var placedTiles: [[Int]: TileRef] = [:]

    // start at a corner
    let firstCorner = tiles.filter({$0.type == .CORNER}).sorted(by: {$0.id < $1.id}).first!
    var nextBorder = firstCorner.borders.filter({ $0.type == .INSIDE}).first!
    placedTileIds.insert(firstCorner.id)
    placedTiles[[0, 0]] = TileRef(firstCorner, nextBorder, .RIGHT, nextBorder.isReversed)

    // build first row
    for x in 1..<width {
        placedBorderValues.insert(nextBorder)
        placedBorderValues.insert(nextBorder.reversed())

        let currTile = allBorders[nextBorder]!.filter({!placedTileIds.contains($0.id)}).first!
        placedTileIds.insert(currTile.id)
        placedTiles[[x, 0]] = TileRef(currTile, nextBorder, .LEFT, nextBorder.isReversed)

        nextBorder = currTile.oppositeBorders[nextBorder]!
    }

    // build each column
    for x in 0..<width {
        var nextBorder = placedTiles[[x, 0]]!.tile.borders.filter({$0.type == .INSIDE && !placedBorderValues.contains($0)}).first!
        for y in 1..<width {
            placedBorderValues.insert(nextBorder)
            placedBorderValues.insert(nextBorder.reversed())

            let currTile = allBorders[nextBorder]!.filter({!placedTileIds.contains($0.id)}).first!
            placedTileIds.insert(currTile.id)
            placedTiles[[x, y]] = TileRef(currTile, nextBorder, .TOP, nextBorder.isReversed)

            nextBorder = currTile.oppositeBorders[nextBorder]!
        }

    }

    print("========================")
    var p = ""
    for y in 0..<width {
        for x in 0..<width {
            p += "[(\(x),\(y)) \(placedTiles[[x, y]]?.description ?? "x" )]"
        }
        p += "\n"
    }
    print(p)

    let bigTile = genBigTile(placedTiles, width)

    return 0
}

func parseId(_ line: String) -> Int {
    Int(line.split(separator: " ")[1].replacingOccurrences(of: ":", with: ""))!
}

func genBigTile(_ tiles: [[Int]: TileRef], _ width: Int) -> [[Character]] {
    var result = "======================\n"
    var bigTile: [[Character]] = []

    for tileY in 0..<width {
        for tileX in 0..<width {
            for lineY in 0..<10 {
                if (tileX == 0) {
                    bigTile.append([])
                }
                for lineX in 0..<10 {
                    let char: Character = tiles[[tileX, tileY]]!.tile.tile[lineY][lineX]
                    bigTile[(tileY * 11) + lineY].append(char)
                }
                bigTile[(tileY * 11) + lineY].append(" ")
            }
            if (tileX == 0) {
                bigTile.append([])
            }
        }
    }

    for line in bigTile {
        result += String(line) + "\n"
    }

    print(result)

    return bigTile
}

class TileRef: CustomStringConvertible {
    let tile: Tile
    let border: Border
    let borderLoc: BorderLoc
    let borderRev: Bool
    init(_ tile: Tile, _ border: Border, _ borderLoc: BorderLoc, _ borderRev: Bool) {
        self.tile = tile
        self.border = border
        self.borderLoc = borderLoc
        self.borderRev = borderRev
    }

    var description: String {
        "\(tile.id) \(border) \(borderLoc) \(borderRev)"
    }
}

class Tile: CustomStringConvertible {
    let id: Int
    var tile: [[Character]]
    var type: TileType
    var borders: [Border]
    var oppositeBorders: [Border: Border]
    var adjacentBorders: [Border: [Border]]
    var allBorders: [Border: BorderLoc]

    init(_ id: Int, _ tile: [[Character]]) {
        self.id = id
        self.type = .UNKNOWN
        self.tile = tile
        oppositeBorders = [:]

        let top = Border(String(id), tile[0])
        let bottom = Border(String(id), tile[tile.count - 1])
        let left = Border(String(id), tile.map({$0[0]}))
        let right = Border(String(id), tile.map({$0[tile[0].count - 1]}))
        borders = [
            top,
            bottom,
            left,
            right
        ]
        allBorders = [
            top: .TOP, top.reversed(): .TOP,
            bottom: .BOTTOM, bottom.reversed(): .BOTTOM,
            left: .LEFT, left.reversed(): .LEFT,
            right: .RIGHT, right.reversed(): .RIGHT,
        ]
        oppositeBorders = [
            left: right,
            right: left,
            top: bottom,
            bottom: top,
            left.reversed(): right.reversed(),
            right.reversed(): left.reversed(),
            top.reversed(): bottom.reversed(),
            bottom.reversed(): top.reversed(),
        ]
        adjacentBorders = [
            left: [top, bottom],
            right: [top, bottom],
            top: [left, right],
            bottom: [left, right],
            left.reversed(): [top.reversed(), bottom.reversed()],
            right.reversed(): [top.reversed(), bottom.reversed()],
            top.reversed(): [left.reversed(), right.reversed()],
            bottom.reversed(): [left.reversed(), right.reversed()],
        ]
    }

    func getTiles(_ tileRef: TileRef) -> [[Character]] {
        let borderLoc = allBorders[tileRef.border]!
        if borderLoc == tileRef.borderLoc {
            return tile
        }

        return tile
    }

    var description: String {
        var line = "[\(id) \(type)]"
//        for t in tile {
//            line += "\(String(t))\n"
//        }
        return line
    }
}

class Border: Hashable, CustomStringConvertible {
    let id: String
    let value: [Character]
    let isReversed: Bool
    var type: BorderType

    init(_ id: String, _ value: [Character], _ isReversed: Bool = false) {
        self.id = id
        self.value = value
        self.isReversed = isReversed
        type = .UNKNOWN
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(value)
    }

    func reversed() -> Border {
        var b = Border(id, Array(value.reversed()), !isReversed)
        b.type = type
        return b
    }

    static func ==(lhs: Border, rhs: Border) -> Bool {
        lhs.value == rhs.value
    }

    var description: String {
        String(value) + (isReversed ? "R" : "")
    }
}

enum BorderLoc {
    case LEFT
    case TOP
    case RIGHT
    case BOTTOM
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
