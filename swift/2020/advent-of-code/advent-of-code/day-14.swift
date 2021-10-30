import Foundation

func day14Part1() -> Int {
    let input = readInput(14)

    var values: [Int: Int] = [:]
    var currMask = ""
    for line in input {
        if line.contains("mask") {
            currMask = String(line.split(separator: " ").last!)
        } else {
            let write = parseLine(line: line, mask: currMask)
            values[write.address.intValue] = write.valuePart1()
        }
    }

    return values.values.reduce(0, +)
}

func day14Part2() -> Int {
    let input = readInput(14)

    var values: [Int: Int] = [:]
    var currMask = ""
    for line in input {
        if line.contains("mask") {
            currMask = String(line.split(separator: " ").last!)
        } else {
            let write = parseLine(line: line, mask: currMask)
            for address in write.addressesPart2() {
                values[address] = write.value.intValue
            }
        }
//        print("------------------")
    }

    return values.values.reduce(0, +)
}

func parseLine(line: String, mask: String) -> Write {
    let parts = line.components(separatedBy: "] = ")
    let address = Int(parts[0].dropFirst(4))!
    let value = Int(parts[1])!
    let write = Write(mask: BitObj(mask), value: BitObj(value), address: BitObj(address))
    return write
}


struct Write: CustomStringConvertible {
    let mask: BitObj
    let value: BitObj
    let address: BitObj

    var description: String {
        "[\(address)]=\(value) \(mask)"
    }

    func valuePart1() -> Int {
        value.applyPart1(mask: mask).intValue
    }

    func addressesPart2() -> [Int] {
//        print(" a \(address)")
        let maskedAddress = address.applyPart2(mask: mask)
//        print("ma \(maskedAddress)")
        let numFloats = Int(pow(2.0, Double(maskedAddress.numX)))
        var addresses: [Int] = []
        for i in 0..<numFloats {
            let floatValues = padZeroLeft(String(i, radix: 2), num: maskedAddress.numX)
//            print(floatValues)
            let address = maskedAddress.fillFloat(floatValues: floatValues)
//            print(address)
            addresses.append(address.intValue)
        }
        return addresses
    }
}

func padZeroLeft(_ value: String, num: Int) -> [Character] {
    String(value.reversed()).padding(toLength: num, withPad: "0", startingAt: 0).reversed()
}

struct BitObj: CustomStringConvertible {
    let value: [Character]

    init(_ value: [Character]) {
        self.value = value
    }

    init(_ value: String) {
        self.value = Array(value)
    }

    init(_ value: Int) {
        self.value = padZeroLeft(String(value, radix: 2), num: 36)
    }

    var description: String {
        let dec: Int = Int(String(value), radix: 2) ?? 0
        return "\(String(value)) \(dec)"
    }

    var intValue: Int {
        Int(String(value), radix: 2)!
    }

    var numX: Int {
        value.filter({$0 == "X"}).count
    }

    func applyPart1(mask: BitObj) -> BitObj {
        var result: [Character] = []
        for i in 0..<mask.value.count {
            let c: Character
            if (mask.value[i] == "X") {
                result.append(value[i])
            } else {
                result.append(mask.value[i])
            }
        }
        return BitObj(result)
    }

    func applyPart2(mask: BitObj) -> BitObj {
        var result: [Character] = []
        for i in 0..<mask.value.count {
            let c: Character
            if (mask.value[i] == "X") {
                result.append("X")
            } else if (mask.value[i] == "0") {
                result.append(value[i])
            } else if (mask.value[i] == "1") {
                result.append("1")
            }
        }
        return BitObj(result)
    }

    func fillFloat(floatValues: [Character]) -> BitObj {
        var result: [Character] = []
        var numX = 0
        for i in 0..<value.count {
            let c: Character
            if (value[i] == "X") {
                result.append(floatValues[numX])
                numX += 1
            } else {
                result.append(value[i])
            }
        }
        return BitObj(result)
    }
}
