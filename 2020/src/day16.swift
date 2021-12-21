import Foundation
import utils


func parseField(_ line: String) -> (String, ClosedRange<Int>, ClosedRange<Int>) {
    let pattern = "^([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$"

    let match = line.groups(for: pattern)[0]

    let field = match[1]

    func extractRange(_ a: String, _ b: String) -> ClosedRange<Int> {
        return (Int(a) ?? 0)...(Int(b) ?? 0)
    }
    
    let firstRange = extractRange(match[2], match[3])
    let secondRange = extractRange(match[4], match[5])

    return (field, firstRange, secondRange)
}

func part1(_ tickets: [[Int]], _ fields: [(String, ClosedRange<Int>, ClosedRange<Int>)]) -> Int {

    var errorRate = 0

    for ticket in tickets {
        for val in ticket {
            if !fields.contains(where: {$0.1.contains(val) || $0.2.contains(val)}) {
                errorRate += val
            }
        }
    }

    return errorRate
}

func part2(_ myTicket: [Int], _ nearbyTickets: [[Int]], _ fields: [(String, ClosedRange<Int>, ClosedRange<Int>)]) -> Int {

    let validTickets = nearbyTickets.filter({part1([$0], fields) == 0})
    let m = validTickets.count
    let n = validTickets[0].count

    var fieldToIdxs = [String: [Int]]()
    
    for (field, rangeA, rangeB) in fields {
        for j in 0..<n {
            let column = (0..<m).map({i in validTickets[i][j]})
            if column.allSatisfy({val in rangeA.contains(val) || rangeB.contains(val)}) {
                if fieldToIdxs[field] == nil {
                    fieldToIdxs[field] = [j]
                }
                else {
                    fieldToIdxs[field]!.append(j)
                }               
            }
        }
    }
    
    var idxToField = [Int: String]()
    for (field, idxs) in fieldToIdxs.sorted(by: {$0.1.count < $1.1.count}) {
        for idx in idxs {
            if idxToField[idx] == nil {
                idxToField[idx] = field
            }
        }
    }

    let departureVals = idxToField.filter({$0.value.contains("departure")}).map({myTicket[$0.key]})
    return departureVals.reduce(1,*)
}

let fileComponents = getFile(getFilename(day: 16)).components(separatedBy: "\n\n")

let fields = fileComponents[0].split(separator: "\n").map({parseField(String($0))})
let myTicket = fileComponents[1].components(separatedBy: ":\n")[1].split(separator: ",").map({Int($0) ?? 0})
let nearbyTickets = fileComponents[2].components(separatedBy: ":\n")[1].split(separator: "\n").map(
                                                                 {$0.split(separator: ",").map({Int($0) ?? 0})})

print("Part 1")
print("--", part1(nearbyTickets, fields))
print("Part 2")
print("--", part2(myTicket, nearbyTickets, fields))
