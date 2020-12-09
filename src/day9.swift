import Foundation
import utils


func part1(_ numbers: [Int]) -> Int {

    func validNumber(_ numbers: ArraySlice<Int>, addTo: Int) -> Bool {

        let numSet = Set(numbers)

        for num in numbers {
            if numSet.contains(addTo - num) {
                return true
            }
        }
        return false
    }
    
    var idx = 25

    while validNumber(numbers[(idx-25)...(idx-1)], addTo: numbers[idx]) {
        idx += 1
    }

    return numbers[idx]
}

func part2(_ numbers: [Int], target: Int) -> Int {

    func findList(_ idx: Int) -> [Int]? {
        var sum = numbers[idx]
        var idx2 = idx + 1
        
        for j in (idx+1)..<numbers.count {
            sum += numbers[j]
            if sum == target {
                idx2 = j
                break
            }
            if sum > target {
                return nil
            }
        }
        return Array(numbers[idx...idx2])
    }

    for idx in 0..<(numbers.count - 1) {
        if let list = findList(idx) {
            return list.max()! + list.min()!
        }
    }
    
    return 0
}

let numbers = getLines(from: getFilename(day: 9)).map { Int($0) ?? 0}
let invalidNumber = part1(numbers)
print("Part 1")
print("--", invalidNumber)
print("Part 2")
print("--", part2(numbers, target: invalidNumber))
