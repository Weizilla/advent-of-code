import Foundation

func day08Part1() -> Int {
    var instructions = readInput(8).map({readInstruction($0)})
//    print(instructions)

    let (register, inf) = run(instructions)
//    print(instructions)

    return register
}

func day08Part2() -> Int {
    var instructions = readInput(8).map({readInstruction($0)})

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
    let type: InstructionType
    let value: Int
    var visited: Bool = false
    var description: String {
        "\(type) \(value) \(visited)"
    }

    init(type: InstructionType, value: Int) {
        self.type = type
        self.value = value
        visited = false
    }
}
