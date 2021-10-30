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
        n2.prev = n1
    }

    printDict(nodes)

    let start = nodes["YOU"]!
    let end = nodes["SAN"]!
    let pathCount = findPath(start, end, &nodes)
    return pathCount - 2
}

private func count(_ node: Node, _ level: Int) -> Int {
    level + node.next.map({count($0, level + 1)}).reduce(0, +)
}

private func findPath(_ start: Node, _ end: Node, _ nodes: inout [String: Node]) -> Int {
    // Dijkstra's algorithm
    var unvisited = Set<Node>(nodes.values)
    var pathCount = Dictionary(uniqueKeysWithValues: nodes.values.map({($0, 1000)}))
    pathCount[start] = 0

    while (!unvisited.isEmpty) {
        let currNode: Node = unvisited.map({($0, pathCount[$0]!)}).min(by: {$0.1 < $1.1})!.0

        let newCount = pathCount[currNode]! + 1
        for node in currNode.all {
            if unvisited.contains(node) {
                pathCount[node] = newCount
            } else {
                let prevCount = pathCount[node]!
                pathCount[node] = min(prevCount, newCount)
            }
        }
        unvisited.remove(currNode)
    }

    return pathCount[end]!
}

private class Node: CustomStringConvertible, Hashable {
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

    var all: [Node] {
        next + (prev != nil ? [prev!] : [])
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(value)
    }

    static func ==(lhs: Node, rhs: Node) -> Bool {
        lhs.value == rhs.value
    }
}
