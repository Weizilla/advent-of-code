import Foundation

func day13Part1() -> Int {
    let input = readInput(13)
    let soonest = Int(input[0])!
    let busIds = input[1].split(separator: ",").filter({i in i != "x"}).map({Int($0)!})
    print("\(soonest) \(busIds)")

    let nextBus = busIds
        .map({i in (i, (soonest / i + 1) * i - soonest)})
        .sorted(by: { $0.1 < $1.1}).first!
    return nextBus.0 * nextBus.1
}

func day13Part2() -> Int {
    let input = readInput(13)
    print(input[1])
    let busIds = input[1].split(separator: ",")
        .enumerated()
        .filter({i in i.1 != "x"})
        .map({ (x, y) in (offset: x, busId: Int(y)!)})

    let maxBus = busIds.sorted(by: {$0.busId > $1.busId}).first!
    var timestamp = maxBus.busId
    while (!isMatch(timestamp: timestamp, maxBus: maxBus, busIds: busIds)) {
        timestamp += maxBus.busId
//        print(timestamp)
    }
    return timestamp
}

func isMatch(timestamp: Int, maxBus: (offset: Int, busId: Int), busIds: [(offset: Int, busId: Int)]) -> Bool {
    let t0 = timestamp - maxBus.offset
    for (offset, busId) in busIds {
        let rem = (t0 + offset) % busId
        if rem != 0 {
            return false
        }
    }
    return true
}
