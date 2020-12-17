import Foundation

func day07Part1() -> Int {
    let input = readInput(7)
    var rules = buildRules(input)

//    print(rules)

    var tree: [String: [String]] = [:]
    for (name, contains) in rules {
        for containName in contains.keys {
            tree.merge([containName: [name]], uniquingKeysWith: +)
        }
    }

//    print(tree)

    var bags: Set<String> = Set(tree["shiny gold"]!)
    countBags(names: bags, tree: tree, bags: &bags)
    return bags.count
}

func day07Part2() -> Int {
    let input = readInput(7)
    var rules = buildRules(input)

//    print(rules)

    return countBags(name: "shiny gold", rules: rules) - 1
}

private func buildRules(_ input: [String]) -> [String: [String: Int]] {
    var rules: [String: [String: Int]] = [:]
    for var line in input {
        let rule = readLine(line)
        rules[rule.0] = rule.1
    }
    return rules
}

func countBags<T: Sequence>(names: T, tree: [String: [String]], bags: inout Set<String>) where T.Element == String {
    for name in names {
        let children = tree[name] ?? []
        if !children.isEmpty {
            bags.formUnion(children)
            countBags(names: children, tree: tree, bags: &bags)
        }
    }
}

func countBags(name: String, rules: [String: [String: Int]]) -> Int {
    let contains = rules[name] ?? [:]
    var count = 1
    for (childName, num) in contains {
//        print("\(childName) \(num)")
        count += num * countBags(name: childName, rules: rules)
    }
    return count
}


func readLine(_ line: String) -> (String, [String: Int]) {
    if line.contains(" bags contain no other bags") {
        let name = line.prefix(upTo: line.range(of: " bags")!.lowerBound)
//        print("END: \(name)")
        return (String(name), [:])
    }

    let cleaned = line
        .replacingOccurrences(of: ".", with: "")
        .replacingOccurrences(of: "contain", with: "")
        .replacingOccurrences(of: ",", with: "")
        .replacingOccurrences(of: "bags", with: "bag")
    let splits = cleaned.components(separatedBy: "bag")
    let name = splits[0].trimmingCharacters(in: .whitespacesAndNewlines)

    var contains: [String: Int] = [:]
    for i in 1..<splits.count {
        let bagLine = splits[i].trimmingCharacters(in: .whitespacesAndNewlines)
        if bagLine.isEmpty {
            continue
        }

//        print("b \(bagLine)");
        let countIndex = bagLine.firstIndex(of: " ")
        let count = Int(String(bagLine.prefix(upTo: countIndex!)))!
        let bagName = String(bagLine.suffix(from: countIndex!)).trimmingCharacters(in: .whitespacesAndNewlines)
//        print("\(count) [\(bagName)]")
        contains[bagName] = count
    }

//    print("\(splits) \(name)")
    return (String(name), contains)
}
