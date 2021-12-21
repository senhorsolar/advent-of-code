import Foundation
import utils


func countOccupied(_ seatMatrix: [[Character]]) -> Int {

    let occupied: Character = "#"
    var nOccupied: Int = 0
    
    for row in seatMatrix {
        nOccupied += row.map({$0 == occupied ? 1 : 0}).reduce(0, +)
    }
    
    return nOccupied
}


func applyRules(_ seatsBefore: [[Character]], part: Int) -> ([[Character]], Bool) {

    var seats = seatsBefore
    
    let occupied: Character = "#"
    let floor: Character = "."
    let empty: Character = "L"
    
    let m = seats.count
    let n = seats[0].count
    
    func getAdjacents(_ i: Int, _ j: Int) -> [Character] {
        var adjacentSeats: [Character] = []
        
        for ii in [i-1, i, i+1] {
            for jj in [j-1, j, j+1] {
                if ii == i && jj == j {
                    //continue
                }

                if ii < 0 || ii >= m || jj < 0 || jj >= n {
                    continue
                }

                adjacentSeats.append(seatsBefore[ii][jj])
            }
        }
        
        return adjacentSeats
    }

    func getVisible(_ i: Int, _ j: Int) -> [Character] {
        func firstInDirection(_ stepX: Int, _ stepY: Int) -> Character {
            let first = empty
            var x = i + stepX , y = j + stepY
            while x >= 0 && x < m && y >= 0 && y < n {
                //print("x: \(x), y: \(y), m: \(m), n: \(n)")
                if seatsBefore[x][y] != floor {
                    return seatsBefore[x][y]
                }
                x += stepX
                y += stepY
            }
            return first
        }

        var visible: [Character] = []
        visible.append(firstInDirection(-1,0))
        visible.append(firstInDirection(-1,1))
        visible.append(firstInDirection(0,1))
        visible.append(firstInDirection(1,1))
        visible.append(firstInDirection(1,0))
        visible.append(firstInDirection(1,-1))
        visible.append(firstInDirection(0,-1))
        visible.append(firstInDirection(-1,-1))
        return visible
    }

    let fnMap = [1: getAdjacents, 2: getVisible]
    
    let emptyFunc = {i, j in return fnMap[part]!(i, j).allSatisfy({$0 != occupied})}
    let occupiedFunc = {i, j in return fnMap[part]!(i, j).map({$0 == occupied ? 1 : 0}).reduce(0,+) >= 5}

    var changed = false

    for i in 0..<m {
        for j in 0..<n {
            let seat = seatsBefore[i][j]
            
            switch seat {
            case empty:
                if emptyFunc(i, j) {
                    seats[i][j] = occupied
                    changed = true
                }
            case occupied:
                if occupiedFunc(i, j) {
                    seats[i][j] = empty
                    changed = true
                }
            default:
                break
            }
        }
    }
    
    return (seats, changed)
}

func solve(_ seatMatrix: [[Character]], part: Int) -> Int {
    
    var (seats, changed) = applyRules(seatMatrix, part: part)
    
    while changed {
        (seats, changed) = applyRules(seats, part: part)
    }

    return countOccupied(seats)
}

let seatMatrix = getLines(from: getFilename(day: 11)).map({Array($0)})

print("Part 1")
print("--", solve(seatMatrix, part: 1))
print("Part 2")
print("--", solve(seatMatrix, part: 2))
