import Foundation

func day02Part1() -> Int {
    var intCodes = readInputInt(2, separator: ",")
    intCodes[1] = 12
    intCodes[2] = 2
    print(intCodes)
    runProgram(intCodes: &intCodes)

    print(intCodes)

    return intCodes[0]
}

func day02Part2() -> Int {
    let intCodes = readInputInt(2, separator: ",")
    for i in 0...99 {
        for j in 0...99 {
            var currCodes = intCodes
            currCodes[1] = i
            currCodes[2] = j
//            print("Trying i=\(i) j=\(j)")
            runProgram(intCodes: &currCodes)
            if currCodes[0] == 19690720 {
                return 100 * i + j
            }
        }
    }
    return 0
}

private func runProgram(intCodes: inout [Int]) {
    var i = 0
    while i < intCodes.count {
        let intCode = intCodes[i]

        if (intCode == 1) {
            let next1 = intCodes[intCodes[i + 1]]
            let next2 = intCodes[intCodes[i + 2]]
            let result = next1 + next2
            intCodes[intCodes[i + 3]] = result
            i += 3
        } else if (intCode == 2) {
            let next1 = intCodes[intCodes[i + 1]]
            let next2 = intCodes[intCodes[i + 2]]
            let result = next1 * next2
            intCodes[intCodes[i + 3]] = result
            i += 3
        } else if intCode == 99 {
            break
        }

        i += 1
    }
}
