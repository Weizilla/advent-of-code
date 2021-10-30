import Foundation

print(day20Part2())


// MARK - input functions

func readInput(_ day: Int, example: Int? = nil, input: Int? = nil, filterEmpty: Bool = true) -> [String] {
    let dayStr = String(format: "%02d", day)
    let fileName = example != nil
        ? "day-\(dayStr)-example-\(example!)"
        : (input != nil ? "day-\(dayStr)-input-\(input!)" : "day-\(dayStr)-input")

    let currentDir = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
    let bundleDir = URL(fileURLWithPath: "advent-of-code-inputs.bundle", relativeTo: currentDir)
    guard let bundle = Bundle(url: bundleDir) else { fatalError("Bundle not found") }
    guard let fileUrl = bundle.url(forResource: fileName, withExtension: "txt") else { fatalError("File url not found: \(fileName)") }
    guard let contentData = FileManager.default.contents(atPath: fileUrl.path) else { fatalError("Content not found") }
    guard let contentString = String(data: contentData, encoding: .utf8) else { fatalError("Content string not found") }
    let contentArray = contentString.components(separatedBy: "\n")
    return filterEmpty ? contentArray.filter({ str in !str.isEmpty }) : contentArray
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
