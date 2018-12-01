import Foundation

func day18Part1() -> Int {
    let lines = readInput(18)

    var result = 0
    for line in lines {
        print(line)
        let parts = Array(line
            .replacingOccurrences(of: "(", with: " ( ")
            .replacingOccurrences(of: ")", with: " ) ")
            .split(separator: " ")
            .map({String($0)})
            .reversed())

        var stack = Stack()

        for i in 0..<parts.count {
            let part = parts[i]
            switch (part) {
            case "(":
                calcTop(&stack)
            default:
                stack.push(part)
            }
        }

        calcTop(&stack)

        result += Int(stack.pop())!
    }
    return result
}

func day18Part2() -> Int {
    let lines = readInput(18)

    var result = 0
    for line in lines {
        print(line)
        let parts = Array(line
            .replacingOccurrences(of: "(", with: " ( ")
            .replacingOccurrences(of: ")", with: " ) ")
            .split(separator: " ")
            .map({String($0)})
            .reversed())

        var stack = Stack()

        for i in 0..<parts.count {
            let part = parts[i]
            switch (part) {
            case "(":
                calcTop2(&stack)
            default:
                stack.push(part)
            }
        }

        calcTop2(&stack)

        result += Int(stack.pop())!
    }
    return result
}

func calcTop(_ stack: inout Stack) {
    var currNum: Int? = nil
    var currSym: Character? = nil
    while (stack.count > 0) {
        let symbol = stack.pop()
        if symbol == ")" {
            break
        }
        if currNum == nil {
            currNum = Int(symbol)
        } else if currSym == nil {
            currSym = Character(symbol)
        } else {
            switch (currSym!) {
            case "*":
                stack.push(currNum! * Int(symbol)!)
            case "+":
                stack.push(currNum! + Int(symbol)!)
            default:
                fatalError()
            }
            currNum = nil
            currSym = nil
        }
        print("Step ================")
        print("num=\(currNum) currSym=\(currSym)")
        print(stack)
        print("---------------------")
    }

    stack.push(currNum!)
}

func calcTop2(_ stack: inout Stack) {
    performAll(&stack, "+")
    performAll(&stack, "*")

    if (stack.count > 1)  {
        let final = stack.pop()
        stack.pop()
        stack.push(final)
    }
}

func performAll(_ stack: inout Stack, _ operation: Character) {
    if stack.count == 1 {
        return
    }

    var newElements = Stack()

    while (stack.elements.count > 0) {
        let element = stack.pop()
        if element == "+" && operation == "+" {
            let last = Int(newElements.pop())!
            let next = Int(stack.pop())!
            newElements.push(String(last + next))
        } else if element == "*" && operation == "*" {
            let last = Int(newElements.pop())!
            let next = Int(stack.pop())!
            newElements.push(String(last * next))
        } else if element == ")" {
            newElements.push(")")
            stack.elements.forEach(newElements.push)
            break
        } else {
            newElements.push(element)
        }
    }

    stack.setElements(newElements.elements.reversed())
}

class Stack: CustomStringConvertible {
    var elements: [String] = []

    func push(_ value: String) {
        elements.append(value)
    }

    func push(_ value: Int) {
        elements.append(String(value))
    }

    func pop() -> String {
        if (elements.isEmpty) {
            fatalError("Empty")
        }
        return elements.removeLast()
    }

    func removeFirst() -> String {
        elements.removeFirst()
    }

    func peek() -> String? {
        elements.last
    }

    var count: Int {
        elements.count
    }

    func setElements(_ elements: [String]) {
        self.elements.removeAll()
        self.elements.append(contentsOf: elements)
    }

    var description: String {
        elements.enumerated()
            .map({(i, e) in "[\(String(i).leftPad(toLength: 2, withPad: " "))] \(e)"})
            .reversed()
            .joined(separator: "\n")
    }
}
