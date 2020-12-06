import Foundation
import utils


func getYesses(_ group: String) -> [Set<Character>] {
    let people = group.split(separator: "\n")

    func forPerson(_ person: Substring) -> Set<Character> {
        var yesses = Set<Character>([])
        for char in person {
            yesses.insert(char)
        }
        return yesses
    }
    
    return people.map {forPerson($0)}
}

func countUnion(_ group: String) -> Int {

    let allYesses = getYesses(group)
    let yesUnion = allYesses.reduce(allYesses[0], {x, y in x.union(y)})
    
    return yesUnion.count
}

func countInter(_ group: String) -> Int {

    let allYesses = getYesses(group)
    let yesInter = allYesses.reduce(allYesses[0], {x, y in x.intersection(y)})

    return yesInter.count
}

let groups = getFile(getFilename(day: 6)).components(separatedBy: "\n\n")
let nYesses = (groups.map {countUnion($0)}).reduce(0, +)
let nAnon = (groups.map {countInter($0)}).reduce(0, +)

print("Part 1")
print("--\(nYesses)")
print("Part 2")
print("--\(nAnon)")
