import Foundation

print(day01Part1())

func getAocRoot() -> String {
    let mainScriptPath: String = #file
    if let swiftPathRange = mainScriptPath.range(of: "swift") {
        let aocRoot = mainScriptPath[..<swiftPathRange.lowerBound]
        return String(aocRoot)
    } else {
        fatalError("AOC root path not found")
    }
}

func readInput(_ day: Int, example: Int? = nil, filterEmpty: Bool = true, year: String = "2022") -> [String] {
    let aocRoot = getAocRoot()
    let dayStr = String(format: "%02d", day)
    let fileName = example != nil ? "day-\(dayStr)-example-\(example!)" : "day-\(dayStr)-input"
    let path = aocRoot + "inputs/\(year)/\(fileName).txt"

    let url = URL(fileURLWithPath: path)
    let contentString = try! String(contentsOf: url, encoding: .utf8)
    let contentArray = contentString.components(separatedBy: "\n")

    return filterEmpty ? contentArray.filter {str in !str.isEmpty} : contentArray
}


func readInputInt(_ day: Int, example: Int? = nil) -> [Int] {
    let strings = readInput(day, example: example)
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

extension String {
    func leftPad(toLength: Int, withPad: String) -> String {
        String(String(reversed()).padding(toLength: toLength, withPad: withPad, startingAt: 0).reversed())
    }
}
