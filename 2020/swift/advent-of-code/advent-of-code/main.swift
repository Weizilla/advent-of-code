//
// Created by Wei Yang on 12/7/20.
//

import Foundation

print(day2Part2())

// MARK - input functions

func readInput(_ day: Int) -> [String] {
    let dayStr = String(format: "%02d", day)
    let fileName = "day-\(dayStr)-input"

    let currentDir = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
    let bundleDir = URL(fileURLWithPath: "advent-of-code-inputs.bundle", relativeTo: currentDir)
    guard let bundle = Bundle(url: bundleDir) else { fatalError("Bundle not found") }
    guard let fileUrl = bundle.url(forResource: fileName, withExtension: "txt") else { fatalError("File url not found") }
    guard let contentData = FileManager.default.contents(atPath: fileUrl.path) else { fatalError("Content not found") }
    guard let contentString = String(data: contentData, encoding: .utf8) else { fatalError("Content string not found") }
    return contentString.components(separatedBy: "\n")
            .filter({ str in !str.isEmpty })
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