import Foundation

func day05Part1() -> Int {
    var intCodes = readInputInt(5, separator: ",")
    printProgram(&intCodes)
    inputInstruction.inputs = [1]
    execute(program: &intCodes)
    printProgram(&intCodes)

    return 0
}

func day05Part2() -> Int {
    var intCodes = readInputInt(5, separator: ",")
    printProgram(&intCodes)
    inputInstruction.inputs = [5]
    execute(program: &intCodes)

    printProgram(&intCodes)
    return 0
}

private let inputInstruction = InputInstruction()

private let instructions = Dictionary(uniqueKeysWithValues: [
    AddInstruction(),
    MultiplyInstruction(),
    PrintInstruction(),
    inputInstruction,
    JumpIfFalseInstruction(),
    JumpIfTrueInstruction(),
    LessThanInstruction(),
    EqualInstruction(),
].map( { ($0.opCode, $0)}))

private func execute(program: inout [Int]) {
    var currIndex = 0
    while currIndex < program.count {
        let fullOpCode = program[currIndex]
        let opCode = fullOpCode % 100
        var jump = false

        if let instruction = instructions[opCode] {
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

            let result = instruction.execute(params: params, program: &program)

            if let writeValue = result.writeValue {
                let writeAddress = program[currIndex + 1]
                program[writeAddress] = writeValue
                currIndex += 1
            }

            if let pointerJump = result.pointerJump {
                currIndex = pointerJump
                jump = true
            }
        } else if opCode == 99 {
            return
        }

        if !jump {
            currIndex += 1
        }
    }
}

private class Instruction {
    var opCode: Int { 0 }
    var numReadParams: Int { 0 }

    func execute(params: [Int], program: inout [Int]) -> Result {
        Result(writeValue: nil, pointerJump: nil)
    }
}

private class AddInstruction : Instruction {
    override var opCode: Int { 1 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        Result(writeValue: params[0] + params[1])
    }
}

private class MultiplyInstruction: Instruction {
    override var opCode: Int { 2 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        Result(writeValue: params[0] * params[1])
    }
}

private class InputInstruction: Instruction {
    override var opCode: Int { 3 }
    override var numReadParams: Int { 0 }
    var inputs: [Int] = []
    var currInput: Int = 0

    override func execute(params: [Int], program: inout [Int]) -> Result {
        let result = inputs[currInput]
        currInput += 1
        return Result(writeValue: result)
    }
}

private class PrintInstruction: Instruction {
    override var opCode: Int { 4 }
    override var numReadParams: Int { 1 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        print("PRINT \(params[0])")
        return Result()
    }
}

private class JumpIfTrueInstruction: Instruction {
    override var opCode: Int { 5 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        params[0] > 0 ? Result(pointerJump: params[1]) : Result()
    }
}

private class JumpIfFalseInstruction: Instruction {
    override var opCode: Int { 6 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        params[0] == 0 ? Result(pointerJump: params[1]) : Result()
    }
}

private class LessThanInstruction: Instruction {
    override var opCode: Int { 7 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        Result(writeValue: params[0] < params[1] ? 1 : 0)
    }
}

private class EqualInstruction: Instruction {
    override var opCode: Int { 8 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        Result(writeValue: params[0] == params[1] ? 1 : 0)
    }
}

private struct Result {
    let writeValue: Int?
    let pointerJump: Int?

    init(writeValue: Int? = nil, pointerJump: Int? = nil) {
        self.writeValue = writeValue
        self.pointerJump = pointerJump
    }
}

private func printProgram(_ program: inout [Int]) {
    var output = ["Codes ", "Index "]
    for (i, code) in program.enumerated() {
        output[0] += "[\(String(i).leftPad(toLength: 3, withPad: " "))] "
        output[1] += " \(String(code).leftPad(toLength: 3, withPad: " "))  "
    }
    print("-------------------------")
    print(output[0])
    print(output[1])
}

private func readModes(_ inputCode: Int) -> [Int: ParameterMode] {
    let chars = Array(String(inputCode).reversed())
    let modes = chars.dropFirst(2)
        .map {Int(String($0))!}
        .map {ParameterMode.parse($0)}
    return Dictionary(uniqueKeysWithValues: modes.enumerated().map {($0.offset, $0.element)})
}

private enum ParameterMode: String, CustomStringConvertible {
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
