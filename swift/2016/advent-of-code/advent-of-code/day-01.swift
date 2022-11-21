import Foundation

private enum Direction {
    case N, E, S, W

    func turnLeft() -> Direction {
        switch self {
        case .N: return .W
        case .E: return .N
        case .S: return .E
        case .W: return .S
        }
    }

    func turnRight() -> Direction {
        switch self {
        case .N: return .W
        case .E: return .N
        case .S: return .E
        case .W: return .S
        }
    }

}

private struct Step : CustomStringConvertible {

    let turn: String
    let distance: Int

    init(_ input: String) {
        print(input)
        turn = String(input.prefix(1))
        distance = Int(input.suffix(1))!
    }

    var description: String {
        "\(turn) \(distance)"
    }
}

func day01Part1() -> Int {
    let input = readInput(1)
    let curr = Direction.N

    let steps = input[0]
        .components(separatedBy: ",")
        .map {Step($0
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .components(separatedBy: "")[0])
        }
    print(steps)

    return 0
}
