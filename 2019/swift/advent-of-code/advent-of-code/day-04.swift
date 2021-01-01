import Foundation

func day04Part1() -> Int {
    let minMax = readInputInt(4, separator: "-")
    let min = minMax[0]
    let max = minMax[1]
//    print("min=\(min) max=\(max)")

    var numValid = 0
    for i0 in 2...6 {
        for i1 in i0...9 {
            for i2 in i1...9 {
                for i3 in i2...9 {
                    for i4 in i3...9 {
                        for i5 in i4...9 {
                            let doubleAdj = i0 == i1 || i1 == i2 || i2 == i3 || i3 == i4 || i4 == i5
                            let num = i0 * 100_000 + i1 * 10_000 + i2 * 1_000 + i3 * 100 + i4 * 10 + i5
                            let inRange = num >= min && num <= max
                            if doubleAdj && inRange {
                                numValid += 1
                            }

                        }
                    }
                }
            }
        }
    }

    return numValid
}

func day04Part2() -> Int {
    let minMax = readInputInt(4, separator: "-")
    let min = minMax[0]
    let max = minMax[1]

    var numValid = 0
    for i0 in 2...6 {
        for i1 in i0...9 {
            for i2 in i1...9 {
                for i3 in i2...9 {
                    for i4 in i3...9 {
                        for i5 in i4...9 {
                            let num = i0 * 100_000 + i1 * 10_000 + i2 * 1_000 + i3 * 100 + i4 * 10 + i5
                            let doubleAdj = isDoubleAdj(i0, i1, i2, i3, i4, i5)
                            let inRange = num >= min && num <= max
                            if doubleAdj && inRange {
                                numValid += 1
                            }
                        }
                    }
                }
            }
        }
    }

    return numValid
}

private func isDoubleAdj(_ value: String) -> Bool {
    let ints = Array(value).map {Int(String($0))!}
    return isDoubleAdj(ints[0], ints[1], ints[2], ints[3], ints[4], ints[5])
}

private func isDoubleAdj(_ i0: Int, _ i1: Int, _ i2: Int, _ i3: Int, _ i4: Int, _ i5: Int) -> Bool {
    (i0 == i1 && i1 != i2)
        || ((i1 == i2 && i2 != i3) && (i1 == i2 && i1 != i0))
        || ((i2 == i3 && i3 != i4) && (i2 == i3 && i2 != i1))
        || ((i3 == i4 && i4 != i5) && (i3 == i4 && i3 != i2))
        || (i4 == i5 && i4 != i3)
}
