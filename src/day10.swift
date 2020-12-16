import Foundation
import utils


func part1(_ numbers: [Int]) -> Int {
    var diff: [Int: Int] = [:]

    var prev = 0
    for num in numbers {
        if diff[num - prev] != nil {
            diff[num - prev]! += 1
        }
        else {
            diff[num - prev] = 1
        }
        prev = num
    }

    return diff[1]! * diff[3]!
}

func part2(_ numbers: [Int]) -> Int {

    var memo: [Int: Int] = [:]
    
    func countArrangements(_ index: Int) -> Int {

        if index >= numbers.count {
            return 0
        }
        
        if index == (numbers.count - 1) {
            return 1
        }

        var nArrangements = 0
        var newIdx = index + 1
        while newIdx < numbers.count && numbers[newIdx] <= (numbers[index] + 3) {
            if let res = memo[newIdx] {
                nArrangements += res
            }
            else {
                let res = countArrangements(newIdx)
                memo[newIdx] = res
                nArrangements += res
            }
            newIdx += 1
        }

        return nArrangements
    }

    return countArrangements(0)
}

var numbers = (getLines(from: getFilename(day: 10)).map {Int($0) ?? 0}).sorted()
numbers.append(numbers[numbers.count - 1] + 3)

print("Part 1")
print("--", part1(numbers))

numbers.insert(0, at: 0)
print("Part 2")
print("--", part2(numbers))
