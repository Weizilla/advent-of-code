import Foundation

func day09Part1() -> Int {
    let intCodes = readInputInt(9, separator: ",")
    print(intCodes)
    let program = IntCode(intCodes)
    let outputs = program.run([1])
    print(outputs)
    return outputs[0]
}

func day09Part2() -> Int {
    let intCodes = readInputInt(9, separator: ",")
    print(intCodes)
    let program = IntCode(intCodes)
    let outputs = program.run([2])
    print(outputs)
    return outputs[0]}
