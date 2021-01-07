import Foundation

func day10Part1() -> Int {
    let input = readInput(10)
    let asteroids = readAsteroids(input)

    var numVisible = [[Int]: Int]()

    for curr in asteroids {
        var deltas = [
            [false, false]: Set<String>(),
            [false, true]: Set<String>(),
            [true, false]: Set<String>(),
            [true, true]: Set<String>(),
        ]
        for check in asteroids {
            if curr == check {
                continue
            }
            let deltaX = check[0] - curr[0]
            let deltaY = check[1] - curr[1]
            let deltaStr = deltaAsString(deltaX, deltaY)
            deltas.merge([[deltaX >= 0, deltaY >= 0]: [deltaStr]], uniquingKeysWith: {$0.union($1)})
        }
        numVisible[curr] = deltas.values.map({$0.count}).reduce(0, +)
    }

    guard let maxVis = numVisible.values.max() else { fatalError()}
    return maxVis
}

func day10Part2() -> Int {
    return 0
}

private func deltaAsString(_ dx: Int, _ dy: Int) -> String {
    String(round(1_000 * Double(dx) / Double(dy)) / 1_000)
}

private func readAsteroids(_ inputs: [String]) -> [[Int]] {
    var asteroids = [[Int]]()
    for (y, line) in inputs.enumerated() {
        for (x, char) in Array(line).enumerated() {
            if char == "#" {
                asteroids.append([x, y])
            }
        }
    }
    return asteroids
}
