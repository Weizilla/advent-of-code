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
    let buses = input[1].split(separator: ",")
        .enumerated()
        .filter({i in i.1 != "x"})
        .map({ (x, y) in Bus(id:Int(y)!, offset: x)})

    print(buses)

    var currBuses = buses

    var offset = currBuses[0].offset
    var period = currBuses[0].id
    print("offset=\(offset) period=\(period)")

    for bus in currBuses.dropFirst() {
        offset = findOffset(offset: offset, period: period, bus: bus)
        period = findPeriod(period: period, bus: bus)
        print("bus=\(bus) offset=\(offset) period=\(period)")
    }

    return offset
}

func bruteForcePrint(buses: [Bus]) {
    print("       \(buses.map({String($0.id).padding(toLength: 3, withPad: " ", startingAt: 0)}).joined(separator: ""))")
    for i in 0..<754020 {
        var matches: Set<Int> = []
        for (n, b) in buses.enumerated() {
            if ((i + b.offset) % b.id == 0) {
                matches.insert(n)
            }
        }
        if (matches.count > 1) {
            var log = "\(i)".padding(toLength: 6, withPad: " ", startingAt: 0)
            for j in 0..<buses.count {
                log += matches.contains(j) ? " x " : "   "
            }
            print(log)
            if (matches.count == buses.count) {
                break
            }
        }
    }
}

func findOffset(offset: Int, period: Int, bus: Bus) -> Int {
    var curr = offset
    while ((curr + bus.offset) % bus.id != 0) {
        curr += period
    }
    return curr
}

func findPeriod(period: Int, bus: Bus) -> Int {
    var curr = period
    while (curr % bus.id != 0) {
        curr += period
    }
    return curr
}

struct Bus: CustomStringConvertible {
    let id: Int
    let offset: Int
    var description: String {
        "(id=\(id) offset=\(offset))"
    }
}
