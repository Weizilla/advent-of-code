import Foundation

func day14Part1() -> Int {
    let input = readInput(14, example: 1)
    let reactions = Dictionary(uniqueKeysWithValues: input.map(parseLine).map({($0.name, $0)}))
    print(reactions)

    var stack = ["FUEL": 1]
    while (!stack.keys.allSatisfy({$0 == "ORE"})) {
        let notOres = stack.filter({$0.key != "ORE"})
        let element = notOres.first!

        let needName = element.key
        let needAmount = stack.removeValue(forKey: needName)!
        let reaction = reactions[needName]!
        let multiplier = Int(floor(Double(needAmount) / Double(reaction.amount)))
        if multiplier > 0 {
            for (consumeName, consumeAmount) in reaction.consumes {
                stack.merge([consumeName: consumeAmount * multiplier], uniquingKeysWith: +)
            }
        }
        let remaining = needAmount % reaction.amount
        if remaining > 0 {
            stack.merge([needName: remaining], uniquingKeysWith: +)
        }

//        print("-------------------")
//        printDict(stack)
    }

    return stack.values.reduce(0, +)
}

func day14Part2() -> Int {
    return 0
}

private func parseLine(_ input: String) -> Reaction {
    let splits = input.components(separatedBy: "=>")

    var consumes = [String: Int]()
    for consumeStr in splits[0].components(separatedBy: ",") {
        let consumerSplits = consumeStr.components(separatedBy: " ").filter({!$0.isEmpty})
        let amount = Int(consumerSplits[0])!
        let name = consumerSplits[1]
        consumes[name] = amount
    }

    let produceAmount = Int(splits[1].split(separator: " ").filter({!$0.isEmpty})[0])!
    let produceName = String(splits[1].split(separator: " ").filter({!$0.isEmpty})[1])

    return Reaction(name: produceName, amount: produceAmount, consumes: consumes)
}

private class Reaction: CustomStringConvertible {
    let consumes: [String: Int]
    let amount: Int
    let name: String

    init(name: String, amount: Int, consumes: [String: Int]) {
        self.name = name
        self.amount = amount
        self.consumes = consumes
    }

    var makesOre: Bool {
        consumes["ORE"] != nil
    }

    var description: String {
        "\(consumes) => \(amount) \(name)"
    }
}
