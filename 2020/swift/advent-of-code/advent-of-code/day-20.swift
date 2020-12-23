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
    var tiles = readTiles(input)
    var allBorders: [Border: [Tile]] = [:]
    var width = Int(sqrt(Double(tiles.count)))

    // all borders for all orientations
    for tile in tiles {
        tile.allBorders.keys.forEach({allBorders.merge([$0: [tile]], uniquingKeysWith: +)})
    }

    // figure out border and tile types
    calcTypes(allBorders: &allBorders, tiles: &tiles)

//    for tile in tiles {
//        print(tile)
//    }

    var placedTiles = buildGrid(&allBorders, &tiles, width)

    printTiles(&placedTiles, width)

    let bigTile = buildBigTile(&placedTiles, width)

    // print big tile
    var result = ""
    for line in bigTile {
        result += String(line) + "\n"
    }
    print(result)

    return 0
}

func parseId(_ line: String) -> Int {
    Int(line.split(separator: " ")[1].replacingOccurrences(of: ":", with: ""))!
}

func calcTypes(allBorders: inout [Border: [Tile]], tiles: inout [Tile]) {
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
}

func buildFirstRow(_ allBorders: inout [Border: [Tile]], _ tiles: inout [Tile], _ width: Int) -> ([Tile], Set<Border>) {
    var firstRow: [Tile] = []
    var firstRowIds: Set<Int> = []
    var firstRowBorders: Set<Border> = []

    // start at a corner
    let firstCorner = tiles.filter({$0.type == .CORNER}).sorted(by: {$0.id < $1.id}).first!
    var nextBorder = firstCorner.borders.filter({ $0.type == .INSIDE}).first!
    firstRow.append(firstCorner)
    firstRowIds.insert(firstCorner.id)

    for x in 1..<width {
        firstRowBorders.insert(nextBorder)
        firstRowBorders.insert(nextBorder.reversed())

        let currTile = allBorders[nextBorder]!.filter({!firstRowIds.contains($0.id)}).first!
        firstRowIds.insert(currTile.id)
        firstRow.append(currTile)

        nextBorder = currTile.oppositeBorders[nextBorder]!
    }

    return (firstRow, firstRowBorders)
}

func buildGrid(_ allBorders: inout [Border: [Tile]], _ tiles: inout [Tile], _ width: Int) -> [[Int]: TileRef] {
    let (firstRow, firstRowBorders) = buildFirstRow(&allBorders, &tiles, width)

    var placedTileIds: Set<Int> = []
    var placedTiles: [[Int]: TileRef] = [:]

    // build each column
    for x in 0..<width {
        let topTile = firstRow[x]
        var nextBorder = topTile.borders.filter({$0.type == .INSIDE && !firstRowBorders.contains($0)}).first!
        let topBorder = topTile.oppositeBorders[nextBorder]!
        placedTileIds.insert(topTile.id)
        placedTiles[[x, 0]] = TileRef(topTile, topBorder)

        for y in 1..<width {
            let currTile = allBorders[nextBorder]!.filter({!placedTileIds.contains($0.id)}).first!
            placedTileIds.insert(currTile.id)
            placedTiles[[x, y]] = TileRef(currTile, nextBorder)

            nextBorder = currTile.oppositeBorders[nextBorder]!
        }
    }

    return placedTiles
}

func printTiles(_ tiles: inout [[Int]: TileRef], _ width: Int) {
    print("========================")
    var p = ""
    for y in 0..<width {
        for x in 0..<width {
            p += "[(\(x),\(y)) \(tiles[[x, y]]?.description ?? "x" )]"
        }
        p += "\n"
    }
    print(p)
}

func buildBigTile(_ tiles: inout [[Int]: TileRef], _ width: Int) -> [[Character]] {
    var result = "======================\n"
    var bigTile: [[Character]] = []

    for tileY in 0..<width {
        for tileX in 0..<width {
            for lineY in 0..<10 {
                if (tileX == 0) {
                    bigTile.append([])
                }
                for lineX in 0..<10 {
                    let tileRef = tiles[[tileX, tileY]]!
                    let char = tileRef.tile.getCharacter(x: lineX, y: lineY, tileRef)
                    bigTile[(tileY * 11) + lineY].append(char)
                }
                bigTile[(tileY * 11) + lineY].append(" ")
            }
            if (tileX == 0) {
                bigTile.append([])
            }
        }
    }

    return bigTile
}

class TileRef: CustomStringConvertible {
    let tile: Tile
    let topBorder: Border

    init(_ tile: Tile, _ topBorder: Border) {
        self.tile = tile
        self.topBorder = topBorder
    }

    var description: String {
        "\(tile.id) \(topBorder)"
    }
}

class Tile: CustomStringConvertible {
    let id: Int
    var tile: [[Character]]
    var type: TileType
    var borders: [Border]
    var oppositeBorders: [Border: Border]
    var allBorders: [Border: BorderOrientation]
    let top: Border
    let bottom: Border
    let left: Border
    let right: Border
    let topR: Border
    let bottomR: Border
    let leftR: Border
    let rightR: Border

    init(_ id: Int, _ tile: [[Character]]) {
        self.id = id
        self.type = .UNKNOWN
        self.tile = tile
        oppositeBorders = [:]

        top = Border(String(id), tile[0])
        bottom = Border(String(id), tile[tile.count - 1])
        left = Border(String(id), tile.map({$0[0]}))
        right = Border(String(id), tile.map({$0[tile[0].count - 1]}))

        topR = top.reversed()
        bottomR = bottom.reversed()
        leftR = left.reversed()
        rightR = right.reversed()

        borders = [top, bottom, left, right]

        allBorders = [
            top: .TOP, topR: .TOP_R, bottom: .BOTTOM, bottomR: .BOTTOM_R,
            left: .LEFT, leftR: .LEFT_R, right: .RIGHT, rightR: .RIGHT_R,
        ]

        oppositeBorders = [
            left: right, right: left, top: bottom, bottom: top,
            leftR: rightR, rightR: leftR, topR: bottomR, bottomR: topR,
        ]
    }

    func getCharacter(x: Int, y: Int, _ tileRef: TileRef) -> Character {
        let borderLoc = allBorders[tileRef.topBorder]!

        return tile[y][x]
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

enum BorderOrientation {
    case LEFT
    case TOP
    case RIGHT
    case BOTTOM
    case LEFT_R
    case TOP_R
    case RIGHT_R
    case BOTTOM_R
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
