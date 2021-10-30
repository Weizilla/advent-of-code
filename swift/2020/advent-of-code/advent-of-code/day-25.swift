import Foundation

func day25Part1() -> Int {
    let input = readInput(25)
    let cardPub = Int(input[0])!
    let doorPub = Int(input[1])!

    let cardLoopSize = findLoopSize(cardPub)
    let doorLoopSize = findLoopSize(doorPub)

    print("loop size card=\(cardLoopSize) door=\(doorLoopSize)")

    let cardEncKey = runCrypto(subject: doorPub, loopSize: cardLoopSize)
    let doorEncKey = runCrypto(subject: cardPub, loopSize: doorLoopSize)

    print("end key card=\(cardEncKey) door=\(doorEncKey)")

    return cardEncKey
}

func day25Part2() -> Int {
    return 0
}

func findLoopSize(_ publicKey: Int) -> Int {
    var loopSize = 0
    var result = 1
    var subject = 7
    repeat {
        result *= subject
        result = result % 20201227
        loopSize += 1
    } while (result != publicKey)
    return loopSize
}

func runCrypto(subject: Int, loopSize: Int) -> Int {
    var result = 1
    for i in 0..<loopSize {
        result *= subject
        result = result % 20201227
    }
    return result
}
