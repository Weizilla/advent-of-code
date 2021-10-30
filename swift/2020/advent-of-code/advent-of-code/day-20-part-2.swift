import Foundation

var seaMonster = [
    Array("                  # "),
    Array("#    ##    ##    ###"),
    Array(" #  #  #  #  #  #   "),
]

func day20Part2() -> Int {
    let input = readInput(20)
    var tiles = readTiles(input)
    var allBorders: [Border: [Tile]] = [:]
    let width = Int(sqrt(Double(tiles.count / 2)))

    for tile in tiles {
        for border in tile.value.borders.keys {
            allBorders.merge([border: [tile.value]], uniquingKeysWith: +)
        }
    }

    let firstCorner = findStart(&allBorders, &tiles)
    print("first corner=\(firstCorner)")

    var firstRow = buildFirstRow(&allBorders, firstCorner, width)
    print("first \(firstRow)")

    var placedTiles = placeTiles(&allBorders, &firstRow, width)
    printTiles(&placedTiles, width)

    var bigTile = buildBigTile(&placedTiles, width)
    printBigTile(&bigTile)

    var numMonsters = findSeaMonster(&bigTile)
    print("Num monsters: \(numMonsters)")

    var flippedBigTile: [[Character]] = []
    for row in bigTile {
        flippedBigTile.append(row.reversed())
    }

    var flipNum = findSeaMonster(&flippedBigTile)
    numMonsters += flipNum

    let totalHash = bigTile.map({$0.filter({$0 == "#"}).count}).reduce(0, +)
    print("Num monsters: \(numMonsters) total hash = \(totalHash)")

    return totalHash - (numMonsters * 15)
}

func readTiles(_ input: [String]) -> [String: Tile] {
    var tiles: [String: Tile] = [:]
    var currTileId: String? = nil
    var currTile: [[Character]] = []

    for line in input {
        if line.contains("Tile") {
            if currTileId != nil {
                let tile = Tile(currTileId!, currTile, false)
                tiles[tile.id] = tile
            }
            currTileId = String(parseIdDay20(line))
            currTile = []
        } else {
            currTile.append(Array(line))
        }
    }
    if currTileId != nil {
        let tile = Tile(currTileId!, currTile, false)
        tiles[tile.id] = tile
    }

    for tile in tiles.values {
        var flipped = tile.tile
        for (i, line) in flipped.enumerated() {
            flipped[i] = line.reversed()
        }
        let fTile = Tile(tile.id + "f", flipped, true)
        tiles[fTile.id] = fTile
    }

    return tiles
}

func findStart(_ allBorders: inout [Border: [Tile]], _ tiles: inout [String: Tile]) -> TileRef {
    var tilesToNumBorders: [String: Int] = [:]
    for tile in tiles {
        let numBorders = tile.value.borders.keys.map({allBorders[$0]!.count}).reduce(0, +)
        tilesToNumBorders.merge([tile.value.id: numBorders], uniquingKeysWith: +)
    }
//    print("tiles to num: \(tilesToNumBorders)")

    let corners = tilesToNumBorders.filter({(b, ts) in ts == 6}).keys
//    print("corners: \(corners)")

    let corner = tiles[corners.sorted().first!]!
//    print("corner: \(corner)")

    let rightBorder = corner.borders.keys.filter({allBorders[$0]!.count == 2 && allBorders[corner.rightBorders[$0]!]!.count == 2}).first!
//    print("right corner: \(rightBorder)")

    return TileRef(corner, rightBorder, .RIGHT)
}

func buildFirstRow(_ allBorders: inout [Border: [Tile]], _ firstCorner: TileRef, _ width: Int) -> [TileRef] {
    var firstRow: [TileRef] = []

    var currTile = firstCorner.tile
    var nextBorder = firstCorner.border
    var bottom = currTile.rightBorders[nextBorder]!

    firstRow.append(TileRef(currTile, bottom, currTile.borders[bottom]!))
//    print("first \(firstRow) \(firstRowBottomBorders)")

    for x in 1..<width {
        let search = nextBorder.reversed()
        currTile = allBorders[search]!.filter({$0.numId != currTile.numId}).first!
        nextBorder = currTile.oppositeBorders[search]!
        bottom = currTile.rightBorders[nextBorder]!
        firstRow.append(TileRef(currTile, bottom, currTile.borders[bottom]!))
//        print("curr \(currTile) \(nextBorder)")
    }

//    print("first \(firstRow) \(firstRowBottomBorders)")

    return firstRow
}

func placeTiles(_ allBorders: inout [Border: [Tile]], _ firstRow: inout [TileRef], _ width: Int) -> [[Int]: TileRef] {
    var placedTiles: [[Int]: TileRef] = [:]

    for x in 0..<width {
        var currTile = firstRow[x].tile
        var nextBorder = firstRow[x].border
        let top = currTile.oppositeBorders[nextBorder]!
        placedTiles[[0, x]] = TileRef(currTile, top, currTile.borders[top]!)
//        print("placed [0, \(x)] \(placedTiles)")

        for y in 1..<width {
            let search = nextBorder.reversed()
            currTile = allBorders[search]!.filter({ $0.numId != currTile.numId }).first!
            placedTiles[[y, x]] = TileRef(currTile, search, currTile.borders[search]!)
            nextBorder = currTile.oppositeBorders[search]!
//            print("placed [\(y),\(x)] \(placedTiles)")

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
    var bigTile: [[Character]] = []

    for tileY in 0..<width {
        for tileX in 0..<width {
            for lineY in 0..<8 {
                if (tileX == 0) {
                    bigTile.append([])
                }
                for lineX in 0..<8 {
                    let tileRef = tiles[[tileY, tileX]]!
                    let char = tileRef.tile.getCharacter(x: lineX + 1, y: lineY + 1, tileRef)
                    bigTile[(tileY * 8) + lineY].append(char)
                }
            }
        }
    }
    return bigTile
}

func printBigTile(_ bigTile: inout [[Character]]) {
    var result = ""
    for line in bigTile {
        result += String(line) + "\n"
    }
    print(result)
}

func findSeaMonster(_ bigTile: inout [[Character]]) -> Int {
    let maxY = bigTile.count
    let maxX = bigTile[0].count
    var num = 0

    for orientation in Orientation.allCases {
        for lineY in 0..<maxY {
            var gridRow: [Character] = []
            for lineX in 0..<maxX {
                let gridChar = getGridChar(&bigTile, x: lineX, y: lineY, orientation)
                gridRow.append(gridChar)

                var monster = true
//                print("start at \(lineX), \(lineY)")
                for mY in 0..<seaMonster.count {
                    for mX in 0..<seaMonster[mY].count {
                        let isMonsterPart = seaMonster[mY][mX] == "#"
                        let checkY = lineY + mY
                        let checkX = lineX + mX
                        if (checkY >= maxY || checkX >= maxX) {
                            monster = false
                        } else if (isMonsterPart) {
                            let isSeaHash = getGridChar(&bigTile, x: checkX, y: checkY, orientation) == "#"
                            if (!isSeaHash) {
                                monster = false
                            }
                        }
                    }
                }
                if monster {
                    num += 1
                }
            }
        }
    }

    return num
}

class Tile: CustomStringConvertible, Equatable {
    let id: String
    let numId: Int
    var tile: [[Character]]
    let flipped: Bool
    let borders: [Border: Orientation]
    let oppositeBorders: [Border: Border]
    let rightBorders: [Border: Border]
    let leftBorders: [Border: Border]
    private let top: Border
    private let right: Border
    private let bottom: Border
    private let left: Border

    init(_ id: String, _ tile: [[Character]], _ flipped: Bool) {
        self.id = id
        numId = Int(id.replacingOccurrences(of: "f", with: ""))!
        self.tile = tile
        self.flipped = flipped

        top = Border(tile[0])
        right = Border(tile.map({$0[tile[0].count - 1]}))
        bottom = Border(tile[tile.count - 1].reversed())
        left = Border(tile.map({$0[0]}).reversed())
        borders = [top: .TOP, bottom: .BOTTOM, left: .LEFT, right: .RIGHT]
        oppositeBorders = [left: right, right: left, top: bottom, bottom: top]
        rightBorders = [top: right, right: bottom, bottom: left, left: top]
        leftBorders = [top: left, left: bottom, bottom: right, right: top]
    }


    func getCharacter(x: Int, y: Int, _ tileRef: TileRef) -> Character {
        let borderLoc = borders[tileRef.border]!
        // return x y if the case border is at top (if left is at top)
        return getGridChar(&tile, x: x, y: y, borderLoc)
    }

    var description: String {
        "[\(id.leftPad(toLength: 5, withPad: " "))]"
    }

    static func ==(lhs: Tile, rhs: Tile) -> Bool {
        lhs.id == rhs.id
    }
}

func getGridChar(_ grid: inout [[Character]], x: Int, y: Int, _ orientation: Orientation) -> Character {
    let max = grid.count - 1
    switch (orientation) {
    case .TOP: return grid[y][x]
    case .RIGHT: return grid[x][max - y]
    case .LEFT: return grid[max - x][y]
    case .BOTTOM: return grid[max - y][max - x]
    }
}

class Border: Hashable, CustomStringConvertible {
    let value: [Character]

    init(_ value: [Character]) {
        self.value = value
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(value)
    }

    func reversed() -> Border {
        Border(value.reversed())
    }

    static func ==(lhs: Border, rhs: Border) -> Bool {
        lhs.value == rhs.value
    }

    var description: String {
        "\(String(value))"
    }
}

class TileRef: CustomStringConvertible {
    let tile: Tile
    let border: Border
    let borderLoc: Orientation

    init(_ tile: Tile, _ border: Border, _ borderLoc: Orientation) {
        self.tile = tile
        self.border = border
        self.borderLoc = borderLoc
    }

    var description: String {
        "\(tile) \(border) \(borderLoc.rawValue.leftPad(toLength: 6, withPad: " "))"
    }
}

enum Orientation: String, CaseIterable {
    case TOP
    case RIGHT
    case BOTTOM
    case LEFT
}
