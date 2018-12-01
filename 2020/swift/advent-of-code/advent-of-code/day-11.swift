import Foundation

let DELTAS = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
]

func day11Part1() -> Int {
    let input = readInput(11)

    var layout: [[Character]] = []
    for line in input {
        var a = Array(line)
        layout.append(a)
    }

    var changed: Bool
    repeat {
        changed = stepPart1(&layout)
//        print(layout)
    } while (changed)

    return layout.map({$0.filter({$0 == "#"}).count}).reduce(0, +)
}

func day11Part2() -> Int {
    let input = readInput(11)

    var layout: [[Character]] = []
    for line in input {
        var a = Array(line)
        layout.append(a)
    }

//    print(layout)

    var changed: Bool
    repeat {
        changed = stepPart2(&layout)
//        print(layout)
    } while (changed)

    return layout.map({$0.filter({$0 == "#"}).count}).reduce(0, +)
}

func stepPart1(_ layout: inout [[Character]]) -> Bool {
    var changed = false
    let old = layout
    for r in 0..<layout.count {
        for c in 0..<layout[0].count {
            if old[r][c] == "." {
                layout[r][c] = "."
                continue
            }

            var numOcc = 0
            for (rdelta, cdelta) in DELTAS {
                let newR = r + rdelta
                let newC = c + cdelta
                if newR < 0 || newC < 0 || newR >= layout.count || newC >= layout[0].count {
                    continue
                }

                if (old[newR][newC] == "#") {
                    numOcc += 1
                }
            }

            if (numOcc == 0 && old[r][c] == "L") {
                layout[r][c] = "#"
                changed = true
            } else if (numOcc >= 4 && old[r][c] == "#") {
                layout[r][c] = "L"
                changed = true
            }
        }
    }
    return changed
}

func stepPart2(_ layout: inout [[Character]]) -> Bool {
    var changed = false
    let old = layout
    let maxDistance = max(layout.count, layout[0].count)
    for r in 0..<layout.count {
        for c in 0..<layout[0].count {
            if old[r][c] == "." {
                layout[r][c] = "."
                continue
            }

            let numOcc = countOccSeats(row: r, col: c, layout: old, maxDistance: maxDistance)

            if (numOcc == 0 && old[r][c] == "L") {
                layout[r][c] = "#"
                changed = true
            } else if (numOcc >= 5 && old[r][c] == "#") {
                layout[r][c] = "L"
                changed = true
            }
        }
    }
    return changed
}

func countOccSeats(row: Int, col: Int, layout: [[Character]], maxDistance: Int) -> Int {
    var numOcc = 0
    for (rdelta, cdelta) in DELTAS {
        for i in 1..<(maxDistance + 1) {
            let newR = row + (rdelta * i)
            let newC = col + (cdelta * i)
            if newR < 0 || newC < 0 || newR >= layout.count || newC >= layout[0].count {
                break
            } else if (layout[newR][newC] == "#") {
                numOcc += 1
                break
            } else if (layout[newR][newC] == "L") {
                break
            }
        }
    }
    return numOcc
}


func print(_ layout: [[Character]]) {
    print("---------------------")
    layout.forEach({print(String($0))})
}
