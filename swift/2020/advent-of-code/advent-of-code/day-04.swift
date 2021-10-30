import Foundation

let expectedKeys: Set<String> = [
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid",
]

let VALID_HEX: Set<Character> = [
    "a", "b", "c", "d", "e", "f", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
]

let VALID_ECL: Set<String> = [
    "amb", "blu", "brn", "gry", "grn", "hzl", "oth",
]

let VALIDATOR = [
    "by": validByr,
    "iyr": validIyr,
    "eyr": validEyr,
    "hgt": validHgt,
    "hcl": validHcl,
    "ecl": validEcl,
    "pid": validPid,
]

func day04Part1() -> Int {
    let lines = readInput(4, filterEmpty: false)
    let passports = readPassports(lines: lines)
    return passports.filter({allFieldsPresent($0)}).count
}

func allFieldsPresent(_ input: Dictionary<String, String>) -> Bool {
    var keys = Set(input.keys)
    keys.remove("cid")
    return keys == expectedKeys
}

func validByr(_ input: Dictionary<String, String>) -> Bool {
    let value = input["byr"]!
    return value.count == 4 && Int(value)! >= 1920 && Int(value)! <= 2002
}

func validIyr(_ input: Dictionary<String, String>) -> Bool {
    let value = input["iyr"]!
    return value.count == 4 && Int(value)! >= 2010 && Int(value)! <= 2020
}

func validEyr(_ input: Dictionary<String, String>) -> Bool {
    let value = input["eyr"]!
    return value.count == 4 && Int(value)! >= 2020 && Int(value)! <= 2030
}

func validHgt(_ input: Dictionary<String, String>) -> Bool {
    let value = input["hgt"]!
    if value.hasSuffix("cm") {
        if let height = Int(String(value.dropLast(2))) {
            return height >= 150 && height <= 193
        }
    } else if value.hasSuffix("in") {
        if let height = Int(String(value.dropLast(2))) {
            return height >= 59 && height <= 76
        }
    }
    return false
}

func validHcl(_ input: Dictionary<String, String>) -> Bool {
    let value = input["hcl"]!
    if value.hasPrefix("#") && value.count == 7 {
        let hexValue = String(value.dropFirst())
        return hexValue.filter({c in VALID_HEX.contains(c)}).count == 6
    }
    return false
}

func validEcl(_ input: Dictionary<String, String>) -> Bool {
    let value = input["ecl"]!
    return VALID_ECL.contains(value)
}

func validPid(_ input: Dictionary<String, String>) -> Bool {
    let value = input["pid"]!
    return value.count == 9 && Int(value) != nil
}

func readPassports(lines: [String]) -> [Dictionary<String, String>] {
    var passports: Array<Dictionary<String, String>> = []
    var passport: Dictionary<String, String> = [:]

    for line in lines {
        if line.isEmpty {
            var keys = Set(passport.keys)
            keys.remove("cid")
            if !passport.keys.isEmpty {
                passports.append(passport)
            }
            passport = [:]
        } else {
            let fields = line.split(separator: " ")
            for field in fields {
                let parts = field.split(separator: ":")
                let key = String(parts[0])
                let value = String(parts[1])
                passport[key] = value
            }
        }
    }

    return passports
}

func day04Part2() -> Int {
    let lines = readInput(4, filterEmpty: false)
    let passports = readPassports(lines: lines)
    return passports.filter({
        var allValid = true
        if allFieldsPresent($0) {
//            print($0)
            for (name, validator) in VALIDATOR {
                let valid = validator($0)
//                print("\(name) \(valid)")
                allValid = allValid && valid
            }
//            print("-------------")
            return allValid
        }
        return false
    }).count
}
