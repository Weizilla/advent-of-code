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
    let input = readInput(23, example: 1)

    var cups = input.first!.map({ Int(String($0))! })
    var currentCup: Int
    var pickUp = [0, 0, 0]
    var destination: Int
    var destinationIndex: Int
    let max = cups.max()!
    for i in (max + 1)..<(1000001) {
        cups.append(i)
    }

    for i in 0..<10000000 {
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

    let indexOf1 = cups.firstIndex(of: 1)! + 1
    let star1: Int = cups[indexOf1 + 1]
    let star2: Int = cups[indexOf1 + 2]
    return star1 * star2
}
