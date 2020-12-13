import Foundation

func day06Part1() -> Int {
    let input = readInput(6, filterEmpty: false)

    var questions: Set<Character> = []
    var totalCount = 0

    for line in input {
        if line.isEmpty {
            totalCount += questions.count
            questions.removeAll()
        } else {
            for char in line {
                questions.insert(char)
            }
        }
    }

    totalCount += questions.count

    return totalCount
}
func day06Part2() -> Int {
    let input = readInput(6, filterEmpty: false)

    var questions: Dictionary<Character, Int> = [:]
    var totalCount = 0
    var records = 0

    for line in input {
        if line.isEmpty {
            totalCount += questions.filter({ (c, i) in i == records}).count
//            print("\(questions) \(records) \(totalCount)")
            questions.removeAll()
            records = 0
        } else {
            for char in line {
                questions.merge([char: 1], uniquingKeysWith: +)
            }
            records += 1
        }
    }

    totalCount += questions.filter({ (c, i) in i == records}).count

    return totalCount
}
