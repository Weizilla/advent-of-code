import Foundation

func day18Part1() -> Int {
    let line = readInput(18, example: 2)

    return line.map(calcResult).reduce(0, +)
}

func calcResult(_ line: String) -> Int {
    let parts = line
        .replacingOccurrences(of: "(", with: " ( ")
        .replacingOccurrences(of: ")", with: " ) ")
        .split(separator: " ")
        .map({String($0)})
    print(parts)

    let stack = Stack()

    parts.forEach(stack.push)

    print(stack)

    return 0
}

class Stack: CustomStringConvertible {
    var elements: [String] = []

    func push(_ value :String) {
        elements.append(value)
    }

    func pop() -> String {
        elements.removeLast()
    }

    var description: String {
        elements.enumerated()
            .map({(i, e) in "[\(String(i).leftPad(toLength: 2, withPad: " "))] \(e)"})
            .reversed()
            .joined(separator: "\n")
    }
}

extension String {
    func leftPad(toLength: Int, withPad: String) -> String {
        String(String(reversed()).padding(toLength: toLength, withPad: withPad, startingAt: 0).reversed())
    }
}
