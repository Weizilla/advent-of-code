import Foundation

func day12Part1() -> Int {
    let lines = readInput(12)
    var moons = lines.map(readMoon)

    let numSteps = 1000
    for i in 0..<numSteps {
        applyGravity(&moons)
        applyVelocity(&moons)
//        printMoons(&moons)
    }

    return moons.map({$0.energy}).reduce(0, +)
}

func day12Part2() -> Int {
    let lines = readInput(12)
    let moons = lines.map(readMoon)

    var moonsX = moons.map({Moon(x:$0.position.x, y: 0, z: 0)})
    let (offsetX, periodX) = calcOffsetPeriod(&moonsX)
    print("offset \(offsetX) period \(periodX)")

    var moonsY = moons.map({Moon(x: 0, y:$0.position.y, z: 0)})
    let (offsetY, periodY) = calcOffsetPeriod(&moonsY)
    print("offset \(offsetY) period \(periodY)")

    var moonsZ = moons.map({Moon(x: 0, y: 0, z:$0.position.z)})
    let (offsetZ, periodZ) = calcOffsetPeriod(&moonsZ)
    print("offset \(offsetZ) period \(periodZ)")

    var firstM = firstMultiplier(periodX, periodY)
    var secondM = firstMultiplier(periodZ, firstM)
    print("fM \(firstM) sM \(secondM)")

    return secondM
}

private func firstMultiplier(_ a: Int, _ b: Int) -> Int {
    let mx = max(a, b)
    let mn = min(a, b)
    var r = mx
    while (r % mn != 0) {
        r += mx
    }
    return r
}

private func calcOffsetPeriod(_ moons: inout [Moon]) -> (Int, Int) {
    var seen = [[Int]: Int]()

    var hash = moons.map({ $0.hash }).reduce([], +)
    var i = 0
    var offset = seen.updateValue(i, forKey: hash)
    while (offset == nil) {
        applyGravity(&moons)
        applyVelocity(&moons)

        i += 1
        hash = moons.map({ $0.hash }).reduce([], +)
        offset = seen.updateValue(i, forKey: hash)
    }

    return (offset!, i - offset!)
}

private func readMoon(_ input: String) -> Moon {
    let pieces = input
        .replacingOccurrences(of: ">", with: "")
        .replacingOccurrences(of: ",", with: "")
        .replacingOccurrences(of: "=", with: " ")
        .components(separatedBy: " ")

    let x = Int(pieces[1])!
    let y = Int(pieces[3])!
    let z = Int(pieces[5])!

    return Moon(x:x, y:y, z:z)
}

private func applyGravity(_ moons: inout [Moon]) {
    for i in 0..<moons.count {
        for j in (i + 1)..<moons.count {
            var m1 = moons[i]
            var m2 = moons[j]
            calVelocity(&m1, &m2)
        }
    }
}

private func calVelocity(_ moon1: inout Moon, _ moon2: inout Moon) {
    if moon1.position.x < moon2.position.x {
        moon1.velocity.x += 1
        moon2.velocity.x -= 1
    } else if (moon1.position.x > moon2.position.x) {
        moon1.velocity.x -= 1
        moon2.velocity.x += 1
    }

    if moon1.position.y < moon2.position.y {
        moon1.velocity.y += 1
        moon2.velocity.y -= 1
    } else if (moon1.position.y > moon2.position.y) {
        moon1.velocity.y -= 1
        moon2.velocity.y += 1
    }

    if moon1.position.z < moon2.position.z {
        moon1.velocity.z += 1
        moon2.velocity.z -= 1
    } else if (moon1.position.z > moon2.position.z) {
        moon1.velocity.z -= 1
        moon2.velocity.z += 1
    }
}

private func applyVelocity(_ moons: inout [Moon]) {
    for moon in moons {
        moon.position.x += moon.velocity.x
        moon.position.y += moon.velocity.y
        moon.position.z += moon.velocity.z
    }
}

private func printMoons(_ moons: inout [Moon]) {
    print("=========================")
    for m in moons {
        print(m)
    }
}

private class Moon: CustomStringConvertible {
    let position: Triple
    let velocity: Triple

    init(x: Int, y: Int, z: Int) {
        position = Triple(x: x, y: y, z: z)
        velocity = Triple(x: 0, y: 0, z: 0)
    }

    var energy: Int {
        position.absSum * velocity.absSum
    }

    var description: String {
        "pos=\(position) vel=\(velocity)"
    }

    var hash: [Int] {
        position.hash + velocity.hash
    }
}

private class Triple: CustomStringConvertible {
    var x: Int
    var y: Int
    var z: Int

    init(x: Int, y: Int, z: Int) {
        self.x = x
        self.y = y
        self.z = z
    }

    var description: String {
        "(x=\(x.description.leftPad(toLength: 3, withPad: " ")) y=\(y.description.leftPad(toLength: 3, withPad: " ")) z=\(z.description.leftPad(toLength: 3, withPad: " ")))"
    }

    var absSum: Int {
        abs(x) + abs(y) + abs(z)
    }

    var hash: [Int] {
        [x, y, z]
    }
}
