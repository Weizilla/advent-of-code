import Foundation

let DIRS = [ActionType.N, ActionType.E, ActionType.S, ActionType.W]

func day12Part1() -> Int {
    let inputs = readInput(12)
    let actions = inputs.map({mapInput($0)})

    var ship = Ship()
    for action in actions {
        ship.doPart1(action: action)
    }

    return abs(ship.x) + abs(ship.y)
}

func day12Part2() -> Int {
    let inputs = readInput(12)
    let actions = inputs.map({mapInput($0)})

    var ship = Ship()
    var waypoint = WayPoint()
    for action in actions {
        if action.type == .F {
            ship.doPart2(amount: action.value, waypoint: waypoint)
        } else {
            waypoint.doPart2(action: action)
        }
        print("\(action) \(ship) \(waypoint)")
    }

    return abs(ship.x) + abs(ship.y)
}


func mapInput(_ line: String) -> Action {
    let letter = String(line.first!)
    let type = ActionType(rawValue: letter)!
    let value = Int(String(line.dropFirst(1)))!
    return Action(type: type, value: value)
}

struct Action: CustomStringConvertible {
    let type: ActionType
    let value: Int
    var description: String {
        "\(type)\(value)"
    }
}

enum ActionType: String {
    case N
    case E
    case S
    case W
    case L
    case R
    case F
}

class Ship: CustomStringConvertible {
    // ^    N
    // |  W   E
    // y    S
    //   x ->
    var heading = ActionType.E
    var x = 0
    var y = 0

    func doPart1(action: Action) {
        switch (action.type) {
        case .N: y += action.value
        case .E: x += action.value
        case .S: y -= action.value
        case .W: x -= action.value
        case .L:
            rotateRight(-action.value)
        case .R:
            rotateRight(action.value)
        case .F:
            forward(action.value)
        }
    }

    private func forward(_ value: Int) {
        switch (heading) {
        case .N: y += value
        case .E: x += value
        case .S: y -= value
        case .W: x -= value
        case .R: break
        case .L: break
        case .F: break
        }
    }

    private func rotateRight(_ value: Int) {
        let dirIndexChange = (value / 90) % 4
        let currDirIndex = DIRS.firstIndex(of: heading)!
        let newDirIndex = (4 + currDirIndex + dirIndexChange) % 4
        heading = DIRS[newDirIndex]
    }

    func doPart2(amount: Int, waypoint: WayPoint) {
        x += amount * waypoint.x
        y += amount * waypoint.y
    }

    var description: String {
        "Ship \(x),\(y)"
    }
}

class WayPoint: CustomStringConvertible {
    // ^    N
    // |  W   E
    // y    S
    //   x ->
    var x = 10
    var y = 1

    func doPart2(action: Action) {
        switch (action.type) {
        case .N: y += action.value
        case .E: x += action.value
        case .S: y -= action.value
        case .W: x -= action.value
        case .L:
            rotateRight(-action.value)
        case .R:
            rotateRight(action.value)
        case .F: break
        }
    }

    func rotateRight(_ amount: Int) {
        let normalizedRotate = (360 + amount) % 360
        switch (normalizedRotate) {
        case 90: (x, y) = (y, -x)
        case 180: (x, y) = (-x, -y)
        case 270: (x, y) = (-y, x)
        default: break
        }
    }

    var description: String {
        "Waypoint \(x),\(y)"
    }
}
