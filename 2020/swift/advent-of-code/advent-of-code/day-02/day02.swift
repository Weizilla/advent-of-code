import Foundation

func day2Part1() -> Int {
//    let passwords = example
    let passwords = readInput(2)
    return passwords.filter({isValid1($0)}).count
}

func day2Part2() -> Int {
//    let passwords = example
    let passwords = readInput(2)
    return passwords.filter({isValid2($0)}).count
}

let example = [
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc",
]

struct Password {
    let num1: Int
    let num2: Int
    let letter: Character
    let password: String

    init(_ input: String) {
        guard let countDash = input.firstIndex(of: "-") else { fatalError("count dash not found") }
        let countDashAfter = input.index(countDash, offsetBy: 1)
        guard let countEnd = input.firstIndex(of: " ") else { fatalError("Count end not found") }
        let letterStart = input.index(countEnd, offsetBy: 1)
        guard let lastSpace = input.lastIndex(of: " ") else { fatalError("Last space") }
        let passwordStart = input.index(lastSpace, offsetBy: 1)

        num1 = Int(input[..<countDash])!
        num2 = Int(input[countDashAfter..<countEnd])!
        letter = input[letterStart]
        password = String(input[passwordStart...])
    }
}

func isValid1(_ input: String) -> Bool {
    let password = Password(input)
    let count = password.password.filter({$0 == password.letter}).count
//    print("[\(input)] count:[\(count)) min:[\(minCount)] max:[\(maxCount)] letter:[\(letter)] password:[\(password)]")
    return password.num1 <= count && count <= password.num2
}

func isValid2(_ input: String) -> Bool {
    let password = Password(input)
    let passwdStr = password.password
    let char1: Character = passwdStr[passwdStr.index(passwdStr.startIndex, offsetBy: password.num1 - 1)]
    let char2: Character = passwdStr[passwdStr.index(passwdStr.startIndex, offsetBy: password.num2 - 1)]
    return (char1 == password.letter) != (char2 == password.letter)
}