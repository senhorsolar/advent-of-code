import Foundation
import utils

func part1(_ line: String) -> Int {
    
    var row = 0, col = 0
    
    for (idx, char) in line.enumerated() {
        switch String(char) {
        case "B":
            row += powInt(2, 6 - idx)
        case "R":
            col += powInt(2, 2 + 7 - idx)
        default:
            break
        }
    }

    let seatId = row * 8 + col
    return seatId
}

func part2(_ lines: [String]) -> Int{

    let seatIds = (lines.map {part1($0)}).sorted()
    
    let minId = seatIds[0]

    for (idx, seatId) in seatIds.enumerated() {
        if (seatId - minId) > idx {
            return seatId - 1
        }
    }
    
    return 0
}

let lines = getLines(from: getFilename(day: 5))

let maxId = (lines.map {part1($0)}).max() ?? 0
let myId = part2(lines)

print("Part 1")
print("--\(maxId)")

print("Part 2")
print("--\(myId)")


