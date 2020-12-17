import Foundation

func day08Part1() -> Int {
    var instructions = readInput(8).map({readInstruction($0)})
//    print(instructions)

    let (register, _) = run(instructions)
//    print(instructions)

    return register
}

func day08Part2() -> Int {
    var instructions = readInput(8).map({readInstruction($0)})

    for i in 0..<instructions.count {
        instructions[i].flipped = true

        let (register, inf) = run(instructions)

        if (inf) {
            instructions.forEach({i in i.reset()})
        } else {
            return register
        }
    }

    return 0
}

func run(_ instructions: [Instruction]) -> (Int, Bool) {
    var register = 0;
    var index = 0;

    while (index < instructions.count) {
        var curr = instructions[index]
        if (curr.visited) {
            return (register, true)
        }

        switch curr.type {
        case .nop:
            index += 1
        case .acc:
            register += curr.value
            index += 1
        case .jmp:
            index += curr.value
        }
        curr.visited = true
    }

    return (register, false)
}

func readInstruction(_ line: String) -> Instruction {
    let splits = line.split(separator: " ")
    let type = InstructionType(rawValue: String(splits[0]))!
    let value = Int(splits[1])!
    return Instruction(type: type, value: value)
}


enum InstructionType: String {
    case nop
    case acc
    case jmp
}

class Instruction : CustomStringConvertible {
    private let _type: InstructionType
    let value: Int
    var visited: Bool = false
    var flipped: Bool = false
    var description: String {
        "\(_type) \(value) \(visited) \(flipped)"
    }

    init(type: InstructionType, value: Int) {
        _type = type
        self.value = value
        visited = false
    }

    var type: InstructionType {
        get {
            switch _type {
            case .acc: return .acc
            case .jmp: return flipped ? .nop : .jmp
            case .nop: return flipped ? .jmp : .nop
            }
        }
    }

    func reset() {
        flipped = false
        visited = false
    }
}
