import Foundation

func day08Part1() -> Int {
    let input = Array(readInput(8)[0])
    let height = 25
    let width = 6
    let depth = input.count / (height * width)
    print("h \(height) w \(width) d \(depth) input \(input)")

    var x = 0
    var y = 0
    var z = 0
    var currInput = 0
    var currNum = [[Int]: Int]()

    while (x < width && y < height && z < depth) {
        let char = Int(String(input[currInput]))!
        currInput += 1

        currNum.merge([[z, char]: 1], uniquingKeysWith: +)

        x += 1
        if x == width {
            x = 0
            y += 1
        }
        if y == height {
            y = 0
            z += 1
        }
    }

    print(currNum)

    let layerFewestZero = currNum
        .filter({$0.key[1] == 0})
        .min(by: {a, b in a.value < b.value})!.key[0]
    let num1 = currNum[[layerFewestZero, 1]]!
    let num2 = currNum[[layerFewestZero, 2]]!

    print("layerFewestZero \(layerFewestZero) \(num1) \(num2)")

    return num1 * num2
}

func day08Part2() -> Int {
    let input = Array(readInput(8)[0])
    let height = 6
    let width = 25
    let depth = input.count / (height * width)
    print("h \(height) w \(width) d \(depth) input \(input)")

    var x = 0
    var y = 0
    var z = 0
    var currInput = 0
    var currNum = [[Int]: Int]()
    var picture = [[Int]: Int]()

    while (x < width && y < height && z < depth) {
        let char = Int(String(input[currInput]))!
        currInput += 1

        picture[[x, y, z]] = char

        currNum.merge([[z, char]: 1], uniquingKeysWith: +)

        x += 1
        if x == width {
            x = 0
            y += 1
        }
        if y == height {
            y = 0
            z += 1
        }
    }

    var finalPicture = [[Int]: Int]()

    for y in 0..<height {
        for x in 0..<width {
            for z in 0..<depth {
                let char = picture[[x, y, z]]!
                if char == 0 || char == 1 {
                    finalPicture[[x, y]] = char
                    break
                }
            }
        }
    }

    var result = "---------------\n"
    for y in 0..<height {
        for x in 0..<width {
            result += finalPicture[[x, y]]! == 0 ? " " : "X"
        }
        result += "\n"
    }

    print(result)


    return 0
}
