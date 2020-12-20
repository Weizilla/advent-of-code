import Foundation

func day16Part1() -> Int {
    let lines = readInput(16)
    let firstSplit = lines.split(separator: "your ticket:")
    let rules: [Rule] = firstSplit[0].flatMap(readRule)
    let secondSplit = firstSplit[1].split(separator: "nearby tickets:")
    let yourTicket = secondSplit[0].map(readTicket)[0]
    let nearbyTickets = secondSplit[1].map(readTicket)

    var allRanges: [ClosedRange<Int>] = []
    for rule in rules {
        allRanges.append(rule.range1)
        allRanges.append(rule.range2)
    }

    let allFields: [Int] = nearbyTickets.flatMap({$0})
    return allFields.filter({f in !allRanges.contains(where: {$0.contains(f)})}).reduce(0, +)
}

func day16Part2() -> Int {
    let lines = readInput(16)
    let firstSplit = lines.split(separator: "your ticket:")
    let rules: [Rule] = firstSplit[0].flatMap(readRule)
    let secondSplit = firstSplit[1].split(separator: "nearby tickets:")
    let yourTicket = secondSplit[0].map(readTicket)[0]
    let nearbyTickets = secondSplit[1].map(readTicket)

//    print("Rules ------------------")
//    for r in rules.sorted(by: {$0.name < $1.name}) {
//        print(r)
//    }
//    print("-------------------------")

    var allRanges: [ClosedRange<Int>] = []
    for rule in rules {
        allRanges.append(rule.range1)
        allRanges.append(rule.range2)
    }

    var validTickets = nearbyTickets.filter({validTicket(fields: $0, allRanges: &allRanges)})
    validTickets.append(yourTicket)

//    print("vt \(validTickets)")

    var fields: [Int: [Int]] = [:]
    for i in 0..<yourTicket.count {
        let allFields = validTickets.map({$0[i]})
        fields[i] = allFields
    }

//    print("fields -----------------")
//    for i in Array(fields.keys).sorted() {
//        print("f \(i) \(fields[i]!.sorted())")
//    }
//    print("-------------------------")

    var ruleNames: [String: [Int]] = [:]
    for (i, fs) in fields {
        var hasRule = false
        for rule in rules {
            if fs.allSatisfy({rule.isValid($0)}) {
                ruleNames.merge([rule.name: [i]], uniquingKeysWith: +)
                hasRule = true
            }
        }
        if !hasRule {
            print("Something missing rule \(i)")
            fatalError()
        }
    }

//    print("rn \(ruleNames)")

    while (ruleNames.values.filter({$0.count > 1}).count > 0) {
        for (name, singleRowIndex) in ruleNames.filter({(k, v) in v.count == 1}) {
            for (key, value) in ruleNames {
                if (value.count > 1) {
                    ruleNames[key] = value.filter({ $0 != singleRowIndex[0] })
                }
            }
        }
//        print(ruleNames)
    }

    var answer = 1
    for (name, indexes) in ruleNames {
        if (name.contains("departure")) {
            answer *= yourTicket[indexes[0]]
        }
    }
    return answer
}

func validTicket(fields: [Int], allRanges: inout [ClosedRange<Int>]) -> Bool {
    fields.allSatisfy({f in allRanges.contains(where: {r in r.contains(f)})})
}

func readRule(_ line: String) -> Rule {
    let split = line.split(separator: ":")
    let name = String(split[0])
    let components = split[1].split(separator: " ")
    let r1 = components[0].split(separator: "-")
    let r2 = components[2].split(separator: "-")
    return Rule(name: name, range1: Int(r1[0])!...Int(r1[1])!, range2: Int(r2[0])!...Int(r2[1])!)
}

func readTicket(_ line: String) -> [Int] {
    line.split(separator: ",").map({Int($0)!})
}

struct Rule: CustomStringConvertible {
    let name: String
    let range1: ClosedRange<Int>
    let range2: ClosedRange<Int>
    var description: String {
        "\(name) \(range1) \(range2)"
    }

    func isValid(_ value: Int) -> Bool {
        range1.contains(value) || range2.contains(value)
    }
}
