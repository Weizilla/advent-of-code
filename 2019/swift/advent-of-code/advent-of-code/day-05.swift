import Foundation

func day05Part1() -> Int {
    var intCodes = readInputInt(5, separator: ",")
    print("before: \(intCodes)")
    var inputs = [1]
    execute(program: &intCodes)

    print("after: \(intCodes)")

    return 0
}

func day05Part2() -> Int {
    return 0
}

let instructions = [
    AddInstruction(),
    MultiplyInstruction(),
    PrintInstruction(),
    InputInstruction([1]),
]

func execute(program: inout [Int]) {
    var currIndex = 0
    while currIndex < program.count {
        let fullOpCode = program[currIndex]
        let opCode = fullOpCode % 100
        for instruction in instructions {
            if instruction.opCode == opCode {
                var params = [Int]()
                if instruction.numReadParams > 0 {
                    let inputParameters = program[currIndex + 1...currIndex + instruction.numReadParams]
                    let modes = readModes(fullOpCode)
                    for (i, inputParam) in inputParameters.enumerated() {
                        let mode = modes[i] ?? .Position
                        let value: Int
                        switch mode {
                        case .Position: value = program[inputParam]
                        case .Intermediate: value = inputParam
                        }
                        params.append(value)
                    }
                }
                currIndex += instruction.numReadParams

                if let result = instruction.execute(params: params, program: &program) {
                    let writeAddress = program[currIndex + 1]
                    program[writeAddress] = result
                    currIndex += 1
                }

            } else if opCode == 99 {
                return
            }
        }
        currIndex += 1
    }
}

class Instruction {
    var opCode: Int { 0 }
    var numReadParams: Int { 0 }
    func execute(params: [Int], program: inout [Int]) -> Int? { nil }
}

class AddInstruction : Instruction {
    override var opCode: Int { 1 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Int? {
        return params[0] + params[1]
    }
}

class MultiplyInstruction: Instruction {
    override var opCode: Int { 2 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Int? {
        params[0] * params[1]
    }
}

class InputInstruction: Instruction {
    override var opCode: Int { 3 }
    override var numReadParams: Int { 0 }
    var inputs: [Int]
    var currInput: Int

    init(_ inputs: [Int]) {
        self.inputs = inputs
        self.currInput = 0
    }

    override func execute(params: [Int], program: inout [Int]) -> Int? {
        let result = inputs[currInput]
        currInput += 1
        return result
    }
}

class PrintInstruction: Instruction {
    override var opCode: Int { 4 }
    override var numReadParams: Int { 1 }

    override func execute(params: [Int], program: inout [Int]) -> Int? {
        print("PRINT \(params[0])")
        return nil
    }
}

private func readModes(_ inputCode: Int) -> [Int: ParameterMode] {
    let chars = Array(String(inputCode).reversed())
    let modes = chars.dropFirst(2)
        .map {Int(String($0))!}
        .map {ParameterMode.parse($0)}
    return Dictionary(uniqueKeysWithValues: modes.enumerated().map {($0.offset, $0.element)})
}

enum ParameterMode: String, CustomStringConvertible {
    case Position
    case Intermediate

    static func parse(_ value: Int) -> ParameterMode {
        switch (value) {
        case 0: return .Position
        case 1: return .Intermediate
        default: fatalError("Unknown \(value)")
        }
    }

    var description: String {
        self.rawValue
    }
}
