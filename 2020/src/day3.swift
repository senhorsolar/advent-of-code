import Foundation
import utils


func countTrees(_ lines: Array<String>, right: Int, down: Int) -> Int {
    var curCol = right, curRow = down, nTrees = 0

    while curRow < lines.count {
        let curLine = lines[curRow]
        let idx = curLine.index(curLine.startIndex, offsetBy: curCol)

        nTrees += Int(curLine[idx] == "#")

        curCol = (curCol + right) % curLine.count
        curRow += down
    }

    return nTrees
}


let lines = getLines(from: getFilename(day: 3))

// Part 1
let part1 = countTrees(lines, right: 3, down: 1)

print("Part 1")
print("--ntrees: \(part1)")

// Part 2
typealias slope = (right: Int, down: Int)
let slopes = [slope(1,1), slope(3,1), slope(5,1), slope(7,1), slope(1,2)]

let part2 = (slopes.map {countTrees(lines, right: $0.right, down: $0.down)}).reduce(1, *)

print("Part 2")
print("--prod: \(part2)")
