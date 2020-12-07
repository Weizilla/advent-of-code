import Foundation

func readInput(_ day: Int) -> [String] {
    let dayStr = String(format: "%02d", day)
    if let input = Bundle.module.path(forResource: "day\(dayStr)-input", ofType: "txt") {
        do {
            let contents = try String(contentsOfFile: input)
            let lines = contents.components(separatedBy: "\n")
            return lines
        } catch {
            fatalError(error.localizedDescription)
        }
    } else {
        fatalError("Unable to find file for day \(day)")
    }
    return []
}

func readIntInput(_ day: Int) -> [Int] {
    let strings = readInput(day)
    return strings
        .filter({str in !str.isEmpty})
        .map({str in toInt(str)})
}

func toInt(_ str: String) -> Int {
    if let maybeInt = Int(str) {
        return maybeInt
    } else {
        fatalError("Not an int: \(str)")
    }
}

print(day01())
