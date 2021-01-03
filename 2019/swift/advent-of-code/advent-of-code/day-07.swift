import Foundation

func day07Part1() -> Int {
    var intCodes = readInputInt(7, separator: ",")
    printProgram(&intCodes)

    let phaseSettings = generatePhases([], Set([0, 1, 2, 3, 4]))

    var maxOutput = 0
    for phaseSetting in phaseSettings {
        let output = runPhase(intCodes: &intCodes, phaseSetting: phaseSetting)
        maxOutput = max(maxOutput, output)
    }

    return maxOutput
}

func day07Part2() -> Int {
    return 0
}

private func generatePhases(_ current: [Int], _ remaining: Set<Int>) -> [[Int]] {
    var results = [[Int]]()
    if remaining.isEmpty {
        return [current]
    }
    for r in remaining {
        let n: [Int] = current + [r]
        let sub = generatePhases(n, remaining.subtracting([r]))
        results.append(contentsOf: sub)
    }
    return results
}

private func runPhase(intCodes: inout [Int], phaseSetting: [Int]) -> Int {
    var prevOutput = 0
    for i in 0..<phaseSetting.count {
        let inputs = [phaseSetting[i], prevOutput]
        let outputs = executeProgram(program: &intCodes, inputs: inputs)
        prevOutput = outputs[0]
    }
//    printProgram(&intCodes)

    return prevOutput
}
