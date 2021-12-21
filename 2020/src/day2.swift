import Foundation
import utils


func splitLine(_ line: String, pattern: String) -> (Int, Int, Character, String) {
    typealias Rule = (lowerBound: Int, upperBound: Int, letter: Character, password: String)
    
    let matches = line.groups(for: pattern)[0]

    return Rule(Int(matches[1]) ?? 0, Int(matches[2]) ?? 0, Character(matches[3]), matches[4])
}

func validPart1(_ line: String, pattern: String) -> Int {
    // For some reason, type aliases aren't being used in functions
    typealias Rule = (lowerBound: Int, upperBound: Int, letter: Character, password: String)
    
    let rule = Rule(splitLine(line, pattern: pattern))

    let charCount = rule.password.map { Int(rule.letter == $0) }.reduce(0, +)

    return Int(charCount >= rule.0 && charCount <= rule.1)
}

func validPart2(_ line: String, pattern: String) -> Int {
    // For some reason, type aliases aren't being used in functions
    typealias Rule = (lowerBound: Int, upperBound: Int, letter: Character, password: String)

    let rule = Rule(splitLine(line, pattern: pattern))

    let charArray = Array(rule.password)
    let firstChar = charArray[rule.lowerBound - 1]
    let secondChar = charArray[rule.upperBound - 1]
    return Int((firstChar == rule.letter) ^ (secondChar == rule.letter))
}

let lines = getLines(from: getFilename(day: 2))
let pattern = "^([0-9]{1,2})-([0-9]{1,2}) ([a-zA-Z]): ([a-zA-Z]+)$"


let nValidPart1 = (lines.map { validPart1($0, pattern: pattern)}).reduce(0, +)
let nValidPart2 = (lines.map { validPart2($0, pattern: pattern)}).reduce(0, +)

print("Part 1")
print("--\(nValidPart1)")
print("Part 2")
print("--\(nValidPart2)")
  
