import Foundation

func day10Part1() -> Int {
    var input = readInputInt(10).sorted()
    input.insert(0, at: 0)
    input.insert(input.last! + 3, at: input.count)

    var diffCounts: [Int:Int] = [:]

    for i in 1..<input.count {
        let diff = input[i] - input[i - 1]
        diffCounts.merge([diff: 1], uniquingKeysWith: +)
        print(diffCounts)
    }
    return diffCounts[1]! * diffCounts[3]!
}

func day10Part2() -> Int {
    var input = readInputInt(10, example: 1).sorted()
    input.insert(0, at: 0)
    input.insert(input.last! + 3, at: input.count)

    var values = Set(input)
//    print(input)

    let paths = buildPaths(0, values: values)

//    print(paths)

    return paths.count
}

func buildPaths(_ root: Int, values: Set<Int>) -> [[Int]] {
    var paths: [[Int]] = []
    for i in 1..<4 {
        let search = i + root
//        print("Search \(search)")
        if values.contains(search) {
            let childPaths = buildPaths(search, values: values)
//            print("Child paths \(childPaths)")
            for childPath in childPaths {
                paths.append([root] + childPath)
            }
        }
    }
    return paths.isEmpty ? [[root]] : paths
}
