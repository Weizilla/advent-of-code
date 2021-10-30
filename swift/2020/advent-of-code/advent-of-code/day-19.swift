import Foundation

func day19Part1() -> Int {
    let lines = readInput(19, example: 1)

    var rules: [Int: Node] = [:]
    var inputs: [String] = []
    for line in lines {
        if line.contains(":") {
            let node = parseLineDay19(line)
            rules[node.key] = node
        } else {
            inputs.append(line)
        }
    }
//    print(rules)
//    print(inputs)

    var numMatch = 0
    for input in inputs {
        let matched = isMatch(input, &rules)
        print("valid=\(matched) \(input)")
        if matched {
            numMatch += 1
        }
    }

    return numMatch
}

func day19Part2() -> Int {
    let lines = readInput(19, input: 2)

    var rules: [Int: Node] = [:]
    var inputs: [String] = []
    for line in lines {
        if line.contains(":") {
            let node = parseLineDay19(line)
            rules[node.key] = node
        } else {
            inputs.append(line)
        }
    }
//    print(rules)
//    print(inputs)

    var numMatch = 0
    for input in inputs {
        let matched = isMatch2(input, &rules)
        print("valid=\(matched) \(input)")
        if matched {
            numMatch += 1
        }
    }

    return numMatch
}

func isMatch(_ input: String, _ rules: inout [Int: Node]) -> Bool {
    let finalIndexes = match(Array(input), [0], [0], &rules)
    return finalIndexes.filter({$0 == input.count}).count > 0
}

func isMatch2(_ input: String, _ rules: inout [Int: Node]) -> Bool {
    var inputArray = Array(input)
    let finalIndexes = match2(&inputArray, [0], [0], &rules, 0, 0)
    return finalIndexes.filter({$0 == input.count}).count > 0
}

func match(_ input: [Character], _ inputIndex: [Int], _ ruleIds: [Int], _ rules: inout [Int: Node]) -> [Int] {
//    print("m input=\(input) input_index=\(inputIndex) rule_id=\(ruleIds)")

    var newIndexes = inputIndex
    for ruleId in ruleIds {
        let rule = rules[ruleId]
        if let l = rule?.literal {
            var matchedIndexes: Set<Int> = []
            for index in newIndexes {
                if (l == input[index]) {
                    matchedIndexes.insert(index + 1)
                }
            }
//            print("matched \(matchedIndexes)")
            newIndexes = Array(matchedIndexes)
        } else {
            var matchedIndexes: Set<[Int]> = []
            for subRuleIds in rule!.nodeRefs {
                let resultingIndexes = match(input, newIndexes, subRuleIds, &rules)
                matchedIndexes.insert(resultingIndexes)
            }
            newIndexes = Array(matchedIndexes.flatMap({$0}))
        }
    }

    return newIndexes
}

func match2(_ input: inout [Character], _ inputIndex: [Int], _ ruleIds: [Int], _ rules: inout [Int: Node], _ num8: Int, _ num11: Int) -> [Int] {
//    print("m input=\(String(input)) input_index=\(inputIndex) rule_id=\(ruleIds)")

    var newIndexes = inputIndex
    var newNum8 = num8
    var newNum11 = num11
    for ruleId in ruleIds {
        if (ruleId == 8) {
//            print("Halt")
            newNum8 += 1
            if (newNum8 == input.count) {
                return inputIndex
            }
        }

        if (ruleId == 11) {
            newNum11 += 1
            if (newNum11 == input.count) {
                return inputIndex
            }
        }

        let rule = rules[ruleId]
        if let l = rule?.literal {
            var matchedIndexes: Set<Int> = []
            for index in newIndexes {
                if (index < input.count && l == input[index]) {
                    matchedIndexes.insert(index + 1)
                }
            }
            newIndexes = Array(matchedIndexes)
        } else {
            var matchedIndexes: Set<[Int]> = []
            for subRuleIds in rule!.nodeRefs {
                let resultingIndexes = match2(&input, newIndexes, subRuleIds, &rules, newNum8, newNum11)
                matchedIndexes.insert(resultingIndexes)
            }
            newIndexes = Array(matchedIndexes.flatMap({$0}))
        }

    }

    return newIndexes
}

func parseLineDay19(_ line: String) -> Node {
    let splits = line.split(separator: ":")
    let key = Int(splits[0])!
    let childrenStr = splits[1]

    if childrenStr.contains("\"") {
        let literal = childrenStr
            .replacingOccurrences(of: " ", with: "")
            .replacingOccurrences(of: "\"", with: "")

        return Node(key, Character(literal))
    } else {
        let subrules = splits[1].split(separator: "|")
        var nodeRefs: [[Int]] = []
        for subrule in subrules {
            let subruleSplits = Array(subrule.split(separator: " ").map({Int($0)!}))
            nodeRefs.append(subruleSplits)
        }
        return Node(key, nodeRefs)
    }
}

class Node: CustomStringConvertible {
    let key: Int
    let literal: Character?
    let nodeRefs: [[Int]]

    init(_ key: Int, _ literal: Character) {
        self.key = key
        self.literal = literal
        nodeRefs = []
    }

    init(_ key: Int, _ nodeRefs: [[Int]]) {
        self.key = key
        literal = nil
        self.nodeRefs = nodeRefs
    }

    var description: String {
        let body: String = literal != nil ? String(literal!) : "\(nodeRefs)"
        return "[Node key=\(key) \(body)]"
    }
}
