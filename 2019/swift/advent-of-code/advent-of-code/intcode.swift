import Foundation

class IntCode {
    var program: [Int: Int]
    var hasHalt: Bool
    var currInstruction = 0
    var relativeBase = 0
    let printIO: Bool

    init(_ program: [Int], printIO: Bool = false) {
        self.program = Dictionary(uniqueKeysWithValues: program.enumerated().map({($0.offset, $0.element)}))
        self.hasHalt = false
        self.currInstruction = 0
        self.relativeBase = 0
        self.printIO = printIO
    }

    func run(_ inputs: [Int]) -> [Int] {
        let inputInstruction = InputInstruction(inputs, printIO: printIO)
        let outputInstruction = OutputInstruction(printIO: printIO)
        let instructions = Dictionary(uniqueKeysWithValues: [
            AddInstruction(),
            MultiplyInstruction(),
            outputInstruction,
            inputInstruction,
            JumpIfFalseInstruction(),
            JumpIfTrueInstruction(),
            LessThanInstruction(),
            EqualInstruction(),
            RelativeDeltaInstruction(),
        ].map( { ($0.opCode, $0)}))

        while !hasHalt {
//            printProgram()

            let fullOpCode = program[currInstruction]!
            let opCode = fullOpCode % 100
            let modes = readModes(fullOpCode)
            var jump = false

            if let instruction = instructions[opCode] {
                if let currInput = instruction as? InputInstruction {
                    if !currInput.hasInputs {
                        break
                    }
                }

                var params = [Int]()
                if instruction.numReadParams > 0 {
                    let inputParameters = (currInstruction + 1...currInstruction + instruction.numReadParams).map({program[$0] ?? 0})
                    for (i, inputParam) in inputParameters.enumerated() {
                        let mode = modes[i] ?? .Position
                        let value: Int
                        switch mode {
                        case .Position: value = program[inputParam] ?? 0
                        case .Intermediate: value = inputParam
                        case .Relative: value = program[relativeBase + inputParam] ?? 0
                        }
                        params.append(value)
                    }
                }
                currInstruction += instruction.numReadParams

                let result = instruction.execute(params: params, program: &program)

                if let writeValue = result.writeValue {
                    let inputParam = program[currInstruction + 1] ?? 0
                    let writeAddress: Int
                    let mode = modes[instruction.numReadParams] ?? .Position
                    switch mode {
                    case .Position: writeAddress = inputParam
                    case .Intermediate: fatalError("intermediate when writing")
                    case .Relative: writeAddress = inputParam + relativeBase
                    }

                    program[writeAddress] = writeValue
//                    print("write \(writeAddress)=\(writeValue)")
                    currInstruction += 1
                }

                if let pointerJump = result.pointerJump {
                    currInstruction = pointerJump
                    jump = true
                }

                if let relativeDelta = result.relativeDelta {
                    relativeBase += relativeDelta
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
        var output = ["Index ", "Codes ", "Curr  ", "Relative \(relativeBase) Current \(currInstruction)"]
        for i in program.keys.sorted() {
            let code = program[i]!
            output[0] += "[\(String(i).leftPad(toLength: 4, withPad: " "))] "
            output[1] += " \(String(code).leftPad(toLength: 4, withPad: " "))  "
            output[2] += i == currInstruction ? "    |  " : "       "
        }
        print("-------------------------")
        output.forEach({print($0)})
    }

}

private class Instruction {
    var opCode: Int { 0 }
    var numReadParams: Int { 0 }

    func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(writeValue: nil, pointerJump: nil)
    }
}

private class AddInstruction : Instruction {
    override var opCode: Int { 1 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(writeValue: params[0] + params[1])
    }
}

private class MultiplyInstruction: Instruction {
    override var opCode: Int { 2 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(writeValue: params[0] * params[1])
    }
}

private class InputInstruction: Instruction {
    override var opCode: Int { 3 }
    override var numReadParams: Int { 0 }
    var inputs: [Int]
    var currInput = 0
    let printIO: Bool

    init(_ inputs: [Int], printIO: Bool) {
        self.inputs = inputs
        self.printIO = printIO
    }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        let result = inputs[currInput]
        if printIO {
            print("INPUT \(result)")
        }
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
    let printIO: Bool
    var outputs: [Int] = []

    init(printIO: Bool) {
        self.printIO = printIO
    }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        if printIO {
            print("OUTPUT \(params[0])")
        }
        outputs.append(params[0])
        return Result()
    }
}

private class JumpIfTrueInstruction: Instruction {
    override var opCode: Int { 5 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        params[0] > 0 ? Result(pointerJump: params[1]) : Result()
    }
}

private class JumpIfFalseInstruction: Instruction {
    override var opCode: Int { 6 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        params[0] == 0 ? Result(pointerJump: params[1]) : Result()
    }
}

private class LessThanInstruction: Instruction {
    override var opCode: Int { 7 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(writeValue: params[0] < params[1] ? 1 : 0)
    }
}

private class EqualInstruction: Instruction {
    override var opCode: Int { 8 }
    override var numReadParams: Int { 2 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(writeValue: params[0] == params[1] ? 1 : 0)
    }
}

private class RelativeDeltaInstruction: Instruction {
    override var opCode: Int { 9 }
    override var numReadParams: Int { 1 }

    override func execute(params: [Int], program: inout [Int: Int]) -> Result {
        Result(relativeDelta: params[0])
    }
}

private struct Result {
    let writeValue: Int?
    let pointerJump: Int?
    let relativeDelta: Int?

    init(writeValue: Int? = nil, pointerJump: Int? = nil, relativeDelta: Int? = nil) {
        self.writeValue = writeValue
        self.pointerJump = pointerJump
        self.relativeDelta = relativeDelta
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
    case Relative

    static func parse(_ value: Int) -> ParameterMode {
        switch (value) {
        case 0: return .Position
        case 1: return .Intermediate
        case 2: return .Relative
        default: fatalError("Unknown \(value)")
        }
    }

    var description: String {
        self.rawValue
    }
}
