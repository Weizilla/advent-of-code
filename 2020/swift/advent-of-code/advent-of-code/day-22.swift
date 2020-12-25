import Foundation

func day22Part1() -> Int {
    let input = readInput(22)
    var player1: [Int] = []
    var player2: [Int] = []
    var currPlayer: [Int] = []
    for line in input {
        if line.contains("Player 1") {
            continue
        }
        if line.contains("Player 2") {
            player1 = currPlayer
            currPlayer = []
            continue
        }
        currPlayer.append(Int(line)!)
    }
    player2 = currPlayer

    print("player1: \(player1) player2: \(player2)")

    while (player1.isEmpty == player2.isEmpty) {
        let top1 = player1.removeFirst()
        let top2 = player2.removeFirst()

        if (top1 > top2) {
            player1.append(contentsOf: [top1, top2])
        } else {
            player2.append(contentsOf: [top2, top1])
        }
//        print("player1: \(player1) player2: \(player2)")
    }

    return player1.isEmpty ? calcScore(&player2) : calcScore(&player1)
}

func day22Part2() -> Int {
    let input = readInput(22)
    var player1: [Int] = []
    var player2: [Int] = []
    var currPlayer: [Int] = []
    for line in input {
        if line.contains("Player 1") {
            continue
        }
        if line.contains("Player 2") {
            player1 = currPlayer
            currPlayer = []
            continue
        }
        currPlayer.append(Int(line)!)
    }
    player2 = currPlayer

    print("player1: \(player1) player2: \(player2)")

    let winner = playGame(&player1, &player2, 0)

    return winner == .ONE ? calcScore(&player1) : calcScore(&player2)
}

func playGame(_ player1: inout [Int], _ player2: inout [Int], _ level: Int) -> Player {
    var prev1: Set<[Int]> = []

    while (player1.isEmpty == player2.isEmpty) {
//        print("[\(level)] player1: \(player1) player2: \(player2)")

        if prev1.contains(player1) {
            return Player.ONE
        } else {
            prev1.insert(player1)
        }

        let top1 = player1.removeFirst()
        let top2 = player2.removeFirst()
//        print("[\(level)] [\(top1) vs \(top2)]")

        var winner: Player
        if top1 <= player1.count && top2 <= player2.count {
            var sub1 = Array(player1[0..<top1])
            var sub2 = Array(player2[0..<top2])
            winner = playGame(&sub1, &sub2, level + 1)
        } else {
            winner = top1 > top2 ? .ONE : .TWO
        }

        if (winner == .ONE) {
            player1.append(contentsOf: [top1, top2])
        } else {
            player2.append(contentsOf: [top2, top1])
        }
//        print("[\(level)] winner \(winner)")
    }

    return player1.isEmpty ? .TWO : .ONE
}


func calcScore(_ player: inout [Int]) -> Int {
    player.enumerated().reduce(0, {(r, i) in r + ((player.count - i.0) * i.1)})
}

enum Player {
    case ONE
    case TWO
}
