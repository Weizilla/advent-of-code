import Foundation

class IntCode {
    var program: [Int]
    var hasHalt: Bool
    var currInstruction = 0

    init(_ program: [Int]) {
        self.program = program
        self.hasHalt = false
        self.currInstruction = 0
    }

    func run(_ inputs: [Int]) -> [Int] {
        let inputInstruction = InputInstruction(inputs)
        let outputInstruction = OutputInstruction()
        let instructions = Dictionary(uniqueKeysWithValues: [
            AddInstruction(),
            MultiplyInstruction(),
            outputInstruction,
            inputInstruction,
            JumpIfFalseInstruction(),
            JumpIfTrueInstruction(),
            LessThanInstruction(),
            EqualInstruction(),
        ].map( { ($0.opCode, $0)}))

        while currInstruction < program.count {
            let fullOpCode = program[currInstruction]
            let opCode = fullOpCode % 100
            var jump = false

            if let instruction = instructions[opCode] {
                if let currInput = instruction as? InputInstruction {
                    if !currInput.hasInputs {
                        break
                    }
                }

                var params = [Int]()
                if instruction.numReadParams > 0 {
                    let inputParameters = program[currInstruction + 1...currInstruction + instruction.numReadParams]
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
                currInstruction += instruction.numReadParams

                let result = instruction.execute(params: params, program: &program)

                if let writeValue = result.writeValue {
                    let writeAddress = program[currInstruction + 1]
                    program[writeAddress] = writeValue
                    currInstruction += 1
                }

                if let pointerJump = result.pointerJump {
                    currInstruction = pointerJump
                    jump = true
                }
            } else if opCode == 99 {
                hasHalt = true
                break
            }

            if !jump {
                currInstruction += 1
            }
        }

        return outputInstruction.outputs
    }


    func printProgram() {
        var output = ["Index ", "Codes "]
        for (i, code) in program.enumerated() {
            output[0] += "[\(String(i).leftPad(toLength: 4, withPad: " "))] "
            output[1] += " \(String(code).leftPad(toLength: 4, withPad: " "))  "
        }
        print("-------------------------")
        print(output[0])
        print(output[1])
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
    var inputs: [Int]
    var currInput = 0

    init(_ inputs: [Int]) {
        self.inputs = inputs
    }

    override func execute(params: [Int], program: inout [Int]) -> Result {
        let result = inputs[currInput]
//        print("INPUT \(result)")
        currInput += 1
        return Result(writeValue: result)
    }

    var hasInputs: Bool {
        currInput < inputs.count
    }
}

private class OutputInstruction: Instruction {
    override var opCode: Int { 4 }
    override var numReadParams: Int { 1 }

    var outputs: [Int] = []

    override func execute(params: [Int], program: inout [Int]) -> Result {
//        print("OUTPUT \(params[0])")
        outputs.append(params[0])
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
