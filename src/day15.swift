import Foundation
import utils


func findLastSpoken(_ input: [Int], upto: Int) -> Int {
    
    var ages = [Int: [Int]]()

    func updateAge(_ lastSpoken: Int, _ turnNum: Int) {
        if ages[lastSpoken] == nil {
            ages[lastSpoken] = [turnNum]
        }
        else {
            ages[lastSpoken] = [ages[lastSpoken]!.last!, turnNum]
        }
    }

    var lastSpoken = 0
    
    for turnNum in (0..<upto) {
        if turnNum < input.count {
            lastSpoken = input[turnNum]
            ages[lastSpoken] = [turnNum]
        }
        else {
            // First time spoken
            if ages[lastSpoken]!.count == 1 {
                lastSpoken = 0
            }
            else {
                lastSpoken = ages[lastSpoken]![1] - ages[lastSpoken]![0]
            }

            updateAge(lastSpoken, turnNum)
        }
    }

    return lastSpoken
}


let input = getLines(from: getFilename(day: 15))[0].split(separator: ",").map({Int($0) ?? 0})


print("Part 1")
print("--", findLastSpoken(input, upto: 2020))
print("Part 2")
print("--", findLastSpoken(input, upto: 30000000))
