import Foundation

func day23Part1() -> Int {
    let input = readInput(23)

    var cups = input.first!.map({Int(String($0))!})
    var currentCup: Int
    var pickUp = [0, 0, 0]
    var destination: Int
    var destinationIndex: Int
    let max = cups.max()!

    for i in 0..<100 {
        currentCup = cups.first!
        pickUp[0] = cups.remove(at: 1)
        pickUp[1] = cups.remove(at: 1)
        pickUp[2] = cups.remove(at: 1)
        destination = currentCup - 1
        while (destination == pickUp[0] || destination == pickUp[1] || destination == pickUp[2] || destination == 0) {
            if destination == 0 {
                destination = max
            } else {
                destination -= 1
            }
        }

//        print("currentCup=\(currentCup) cups=\(cups) pickup=\(pickUp) destination=\(destination)")

        destinationIndex = cups.firstIndex(of: destination)! + 1
        cups.insert(contentsOf: pickUp, at: destinationIndex)

//        print("----- currentCup=\(currentCup) cups=\(cups) pickup=\(pickUp) destination=\(destination)")

        cups.append(cups.removeFirst())
    }

    var result = ""
    let indexOf1 = cups.firstIndex(of: 1)! + 1
    for i in 0..<8 {
        let index = (indexOf1 + i) % 9
        result += String(cups[index])
    }

    return Int(result)!
}

func day23Part2() -> Int {
    let input = readInput(23)

    var cupValues: [Int: Cup] = [: ]
    var currentCup: Cup
    var pickUp: [Cup] = []
    var destinationValue: Int
    var destinationCup: Cup
    (cupValues, currentCup) = buildCups(input)
    let max = Array(cupValues.keys).max()!

//    print(cupValues.count)
//    print(max)
//    printCups(currentCup, limit: 20)

    for i in 0..<10_000_000 {
//        printCups(currentCup)

        if pickUp.count == 0 {
            pickUp = [
                currentCup.nextCup!,
                currentCup.nextCup!.nextCup!,
                currentCup.nextCup!.nextCup!.nextCup!
            ]
        } else {
            pickUp[0] = currentCup.nextCup!
            pickUp[1] = pickUp[0].nextCup!
            pickUp[2] = pickUp[1].nextCup!
        }

        destinationValue = currentCup.value - 1
        while (destinationValue == pickUp[0].value || destinationValue == pickUp[1].value || destinationValue == pickUp[2].value || destinationValue == 0) {
            if destinationValue == 0 {
                destinationValue = max
            } else {
                destinationValue -= 1
            }
        }

        destinationCup = cupValues[destinationValue]!
//        print("destination \(destinationCup)")

        // remove pick up
        let beforePickup = pickUp[0].prevCup
        let afterPickup = pickUp[2].nextCup
        beforePickup!.nextCup = afterPickup
        afterPickup!.prevCup = beforePickup

        // insert pick up
        let afterDestination = destinationCup.nextCup!
        pickUp[0].prevCup = destinationCup
        destinationCup.nextCup = pickUp[0]
        afterDestination.prevCup = pickUp[2]
        pickUp[2].nextCup = afterDestination

        currentCup = currentCup.nextCup!
    }

//    print("final -------------------")
//    printCups(currentCup)

    let star1 = cupValues[1]!.nextCup!.value
    let star2 = cupValues[1]!.nextCup!.nextCup!.value
    print("stars \(star1) \(star2)")
    return star1 * star2
}

func buildCups(_ input: [String]) -> ([Int: Cup], Cup) {
    var cupInputs = input.first!.map({ Int(String($0))! })
    var cupValues: [Int: Cup] = [:]

    for i in (cupInputs.max()! + 1)...1_000_000 {
        cupInputs.append(i)
    }

    var firstCup: Cup?
    var prevCup: Cup?
    var currCup: Cup?

    for i in 0..<cupInputs.count {
        let value = cupInputs[i]
        currCup = Cup(value)
        cupValues[value] = currCup

        if (i == 0) {
            firstCup = currCup
        }

        if prevCup != nil {
            currCup!.prevCup = prevCup!
            prevCup?.nextCup = currCup!
        }

        prevCup = currCup

    }
    currCup!.nextCup = firstCup!
    firstCup!.prevCup = currCup!

    return (cupValues, firstCup!)
}

func printCups(_ cup: Cup, limit: Int? = nil) {
    let start = cup
    var currCup = cup
    var currNum = 0
    repeat {
        print("[\(currNum)] \(currCup) \(currCup == start ? "<--" : "")")
        currCup = currCup.nextCup!
        currNum += 1
        if (currNum == limit) {
            break
        }
    } while (currCup != start)
}

class Cup: CustomStringConvertible, Equatable {
    let value: Int
    var prevCup: Cup?
    var nextCup: Cup?

    init(_ value: Int) {
        self.value = value
        prevCup = nil
        nextCup = nil
    }

    var description: String {
        "\(value) prev=\(prevCup?.value.description ?? "_") next=\(nextCup?.value.description ?? "_" )"
    }

    static func ==(lhs: Cup, rhs: Cup) -> Bool {
        lhs.value == rhs.value
    }
}
