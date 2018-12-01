import Foundation

func day07Part1() -> Int {
    var intCodes = readInputInt(7, separator: ",")

    let phaseSettings = generatePhases([], Set([0, 1, 2, 3, 4]))

    var maxOutput = 0
    for phaseSetting in phaseSettings {
        let output = runPhase1(intCodes: &intCodes, phaseSetting: phaseSetting)
        maxOutput = max(maxOutput, output)
    }

    return maxOutput
}

func day07Part2() -> Int {
    var intCodes = readInputInt(7, separator: ",")
    let program = IntCode(intCodes)
    program.printProgram()

    let phaseSettings = generatePhases([], Set([5, 6, 7 , 8, 9]))

    var maxOutput = 0
    for phaseSetting in phaseSettings {
        let output = runPhase2(intCodes: &intCodes, phaseSetting: phaseSetting)
        maxOutput = max(maxOutput, output)
    }

    return maxOutput
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

private func runPhase1(intCodes: inout [Int], phaseSetting: [Int]) -> Int {
    var prevOutput = 0
    for i in 0..<phaseSetting.count {
        let inputs = [phaseSetting[i], prevOutput]
        let program = IntCode(intCodes)
        let outputs = program.run(inputs)
        prevOutput = outputs[0]
    }
//    printProgram(&intCodes)

    return prevOutput
}

private func runPhase2(intCodes: inout [Int], phaseSetting: [Int]) -> Int {
    let amps = [
        IntCode(intCodes),
        IntCode(intCodes),
        IntCode(intCodes),
        IntCode(intCodes),
        IntCode(intCodes),
    ]
    var prevOutput = 0
    var currAmp = 0
    var allHalted = amps.allSatisfy({$0.hasHalt})

    while (!allHalted) {
        let inputs = currAmp < 5 ? [phaseSetting[currAmp], prevOutput] : [prevOutput]
        let program = amps[currAmp % 5]
        let outputs = program.run(inputs)
        prevOutput = outputs[0]

        allHalted = amps.allSatisfy({$0.hasHalt})
        currAmp += 1
    }

    return prevOutput
}
