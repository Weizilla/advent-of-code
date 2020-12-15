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
        changed = step(&layout)
//        print(layout)
    } while (changed)

    return layout.map({$0.filter({$0 == "#"}).count}).reduce(0, +)
}

func step(_ layout: inout [[Character]]) -> Bool {
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

func print(_ layout: [[Character]]) {
    print("---------------------")
    layout.forEach({print($0)})
}
