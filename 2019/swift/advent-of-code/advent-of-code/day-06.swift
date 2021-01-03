import Foundation

func day06Part1() -> Int {
    let inputs = readInput(6)

    var nodes = [String: Node]()

    for line in inputs {
        let ids = line.components(separatedBy: ")")
        let id1 = ids[0]
        let id2 = ids[1]
        let n1 = nodes[id1, default: Node(id1)]
        nodes[id1] = n1
        let n2 = nodes[id2, default: Node(id2)]
        nodes[id2] = n2
        n1.next.append(n2)
    }

    let head = nodes["COM"]!
    let total = count(head, 0)
    return total
}

func day06Part2() -> Int {
    let inputs = readInput(6, example: 2)

    var nodes = [String: Node]()

    for line in inputs {
        let ids = line.components(separatedBy: ")")
        let id1 = ids[0]
        let id2 = ids[1]
        let n1 = nodes[id1, default: Node(id1)]
        nodes[id1] = n1
        let n2 = nodes[id2, default: Node(id2)]
        nodes[id2] = n2
        n1.next.append(n2)
        n2.prev = n1
    }

    printDict(nodes)

    let head = nodes["COM"]!
    let total = count(head, 0)
    return total
}

private func count(_ node: Node, _ level: Int) -> Int {
    level + node.next.map({count($0, level + 1)}).reduce(0, +)
}

private class Node: CustomStringConvertible {
    let value: String
    var next: [Node]
    var prev: Node?

    init(_ value: String) {
        self.value = value
        self.next = []
        self.prev = nil
    }

    var description: String {
        "\(value) prev=[\(prev?.value.description ?? "")] next=[\(next.map({$0.value}).joined(separator: ","))]"
    }
}
