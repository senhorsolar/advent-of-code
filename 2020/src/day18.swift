import Foundation
import utils


func removeSpaces(_ s: String) -> String {
    return s.replacingOccurrences(of: " ", with: "")
}

func infixToPostfix(_ line: String, preced: (Character) -> Int) -> String {

    var stack = [Character]()
    stack.append("$")

    var postFix = [Character]()

    for c in line {
        if Int(String(c)) != nil {
            postFix.append(c)
        }
        else if c == "(" {
            stack.append("(")
        }
        else if c == ")" {
            while stack.last! != "$" && stack.last! != "(" {
                postFix.append(stack.removeLast())
            }
            if (stack.last! == "(") {
                stack.removeLast()
            }
        }
        else {
            while (stack.last! != "$" && preced(c) <= preced(stack.last!)) {
                postFix.append(stack.removeLast())
            }
            stack.append(c)
        }
    }
    
    while (stack.last != "$") {
        postFix.append(stack.removeLast())
    }

    return String(postFix)
}

func postfixCalculator(_ postFix: String) -> Int {
    var stack = [Int]()

    for c in postFix {
        if let d = Int(String(c)) {
            stack.append(d)
        }
        else {
            let a = stack.removeLast()
            let b = stack.removeLast()
            if c == "+" {
                stack.append(a + b)
            }
            else if c == "*" {
                stack.append(a * b)
            }
        }
    }

    return stack.removeLast()
}

func part1(_ lines: [String]) -> Int {

    func preced(_ c: Character) -> Int {
        switch c {
        case "+", "*":
            return 1
        default:
          return 0
        }
    }

    func eval(_ line: String) -> Int {
        return postfixCalculator(infixToPostfix(removeSpaces(line), preced: preced))
    }

    return lines.map({eval($0)}).reduce(0,+)
}

func part2(_ lines: [String]) -> Int {
    
    func preced(_ c: Character) -> Int {
        switch c {
        case "+":
            return 2
        case "*":
            return 1
        default:
          return 0
        }
    }

    func eval(_ line: String) -> Int {
        return postfixCalculator(infixToPostfix(removeSpaces(line), preced: preced))
    }
    
    return lines.map({eval($0)}).reduce(0,+)
}

let lines = getLines(from: getFilename(day: 18))

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))
