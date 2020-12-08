import Foundation
import utils


func parseLine(_ line: String) -> (String, Int) {
    let pattern = "^(acc|jmp|nop) ([+-][0-9]*)$"
    let matches = line.groups(for: pattern)[0]
    return (matches[1], Int(matches[2]) ?? 0)
}


// Finds value of accumulator before second loop
func part1(_ lines: [String]) -> Int {

    var visitedInstructions = Set<Int>([])
    var index = 0
    var accumulator = 0
    
    while true {
        if visitedInstructions.contains(index) {
            break
        }

        visitedInstructions.insert(index)
        
        let (instruction, arg) = parseLine(lines[index])
        switch instruction {
        case "acc":
            accumulator += arg
            index += 1
        case "nop":
            index += 1
        case "jmp":
            index += arg
        default:
            break
        }
    }
    
    return accumulator
}


func part2(_ lines: [String]) -> Int {

    var visitedInstructions = Set<Int>([])

    // Returns nil if infinite loop, or acc if not
    func helper(_ index: Int, acc: Int, changed: Bool) -> Int? {

        if index == lines.count {
            return acc
        }

        if visitedInstructions.contains(index) {
            return nil
        }

        visitedInstructions.insert(index)

        let (op, arg) = parseLine(lines[index])

        switch op {
        case "acc":
             return helper(index + 1, acc: acc + arg, changed: changed)
            
        case "nop":

            if changed {
                return helper(index + 1, acc: acc, changed: true)
            }
            else {
                return helper(index + 1, acc: acc, changed: false) ?? helper(index + arg, acc: acc, changed: true)
            }

            
        case "jmp":
            if changed {
                return helper(index + arg, acc: acc, changed: true)
            }
            else {
                return helper(index + arg, acc: acc, changed: false) ?? helper(index + 1, acc: acc, changed: true)
            }
            
        default:
            break
        }

        return nil
    }
    
    return helper(0, acc: 0, changed: false)!
}

let lines = getLines(from: getFilename(day: 8))
let acc = part1(lines)
let acc2 = part2(lines)


print("Part 1")
print("--", acc)
print("Part 2")
print("--", acc2)
