import Foundation

func day05Part1() -> Int {
    let intCodes = readInputInt(5, separator: ",")
    let program = IntCode(intCodes)
    program.printProgram()
    let inputs = [1]
    let outputs = program.run(inputs)
    program.printProgram()

    return outputs.last!
}

func day05Part2() -> Int {
    let intCodes = readInputInt(5, separator: ",")
    let program = IntCode(intCodes)
    program.printProgram()
    let inputs = [5]
    let outputs = program.run(inputs)
    program.printProgram()

    return outputs.last!
}
