import Foundation

func day09Part1() -> Int {
    let input = readInputInt(9)
    let preamble = 25
    var window = Window()

    return findInvalid(preamble: preamble, window: window, input: input)
}

func day09Part2() -> Int {
    let input = readInputInt(9)
    let preamble = 25
    var window = Window()

    let invalid = findInvalid(preamble: preamble, window: window, input: input)
    print(invalid)

    var window2 = Window()

    var window2Index = 0
    while (window2.sum() != invalid) {
        if (window2.sum() < invalid) {
            window2.add(input[window2Index])
            window2Index += 1
        } else {
            window2.dropFirst()
        }
//        print(window2)
    }

    let finalValues = window2.values.sorted()

    return finalValues.first! + finalValues.last!
}

func findInvalid(preamble: Int, window: Window, input: [Int]) -> Int {
    for i in 0..<preamble {
        window.add(input[i])
    }

    for i in preamble..<input.count {
        let curr = input[i]
        let isValid = window.canAdd(curr)
        if isValid {
            window.dropFirst()
            window.add(curr)
        } else {
            return curr
        }
    }
    return 0
}

class Window : CustomStringConvertible {
    var values: [Int] = []
    var valuesCount: [Int:Int] = [:]

    func dropFirst() {
        let first = values.first!
        let count = valuesCount[first]! - 1
        if count == 0 {
            valuesCount.removeValue(forKey: first)
        } else {
            valuesCount[first] = count
        }
        values.removeFirst()
    }

    func add(_ input: Int) {
        values.append(input)
        valuesCount.merge([input: 1], uniquingKeysWith: +)
    }

    func canAdd(_ input: Int) -> Bool {
        for v in values {
            let diff = input - v
            if diff > 0 && valuesCount[diff] != nil {
                return true
            }
        }
        return false
    }

    func sum() -> Int {
        values.reduce(0, +)
    }

    var description: String {
        "----------------\n\(values)\n\(valuesCount)"
    }
}
