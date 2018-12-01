import Foundation

func day15Part1() -> Int {
    let nums: [Int] = readInput(15, example: 1)[0]
        .split(separator: ",")
        .map({Int($0)!})

    let limit = 2020

    var lastSpokes: [Int: [Int]] = [:]
    var lastSpoken = 0

    for i in 0..<limit {
        let spoken: Int
        if i < nums.count {
            spoken = nums[i]
        } else {
            if let s = lastSpokes[lastSpoken] {
                if (s.count == 1) {
                    spoken = 0
                } else {
                    spoken = s.last! - s[s.count - 2]
                }
            } else {
                spoken = 0
            }
        }
        if let l = lastSpokes[spoken] {
            lastSpokes[spoken] = [l.last!, i]
        } else {
            lastSpokes[spoken] = [i]
        }
        lastSpoken = spoken
//        print("i=\(i) spoken=\(spoken) lastSpokes=\(lastSpokes))")
    }

    return lastSpoken
}

func day15Part2() -> Int {
    let nums: [Int] = readInput(15)[0]
        .split(separator: ",")
        .map({Int($0)!})

    let limit = 30000000

    var lastSpokes: [Int: [Int]] = [:]
    var lastSpoken = 0

    for i in 0..<limit {
        let spoken: Int
        if i < nums.count {
            spoken = nums[i]
        } else {
            if let s = lastSpokes[lastSpoken] {
                if (s.count == 1) {
                    spoken = 0
                } else {
                    spoken = s[1] - s[0]
                }
            } else {
                spoken = 0
            }
        }
        if var l = lastSpokes[spoken] {
            lastSpokes[spoken] = [l[l.count - 1], i]
        } else {
            lastSpokes[spoken] = [i]
        }
        lastSpoken = spoken
//        print("i=\(i) spoken=\(spoken) lastSpokes=\(lastSpokes))")
    }

    return lastSpoken
}
