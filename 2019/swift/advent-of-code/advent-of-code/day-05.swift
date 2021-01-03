import Foundation

func day05Part1() -> Int {
    var intCodes = readInputInt(5, separator: ",")
    printProgram(&intCodes)
    let inputs = [1]
    let outputs = executeProgram(program: &intCodes, inputs: inputs)
    printProgram(&intCodes)

    return outputs.last!
}

func day05Part2() -> Int {
    var intCodes = readInputInt(5, separator: ",")
    printProgram(&intCodes)
    let inputs = [5]
    let outputs = executeProgram(program: &intCodes, inputs: inputs)

    printProgram(&intCodes)
    return outputs.last!
}
