import Foundation
import utils

let numbers = getLines(from: getFilename(day: 1)).map {Int($0) ?? 0}
let targetSum = 2020

// Store numbers in dict based on idx
var dict: [Int: Int] = [:]
for (idx, num) in numbers.enumerated() {
    dict[num] = idx
}

func part1(_ numbers: Array<Int>) -> Int {
    
    for (idx, num) in numbers.enumerated() {
        if let idx2 = dict[targetSum - num] {
            if idx2 != idx {
                let otherNum = targetSum - num
                return num * otherNum
            }
        }
    }
    return 0
}

func part2(_ numbers: Array<Int>) -> Int {
    
    for (idx1, num1) in numbers.enumerated() {
        for (idx2, num2) in numbers.enumerated() {
            if idx2 != idx1 {
                let num3 = targetSum - num1 - num2
                if let idx3 = dict[num3] {
                    if idx3 != idx1 || idx3 != idx2 {
                        return num1 * num2 * num3
                    }
                }
            }
        }
    }
    
    return 0
}

print("Part 1")
print("--\(part1(numbers))")
print("Part 2")
print("--\(part2(numbers))")
