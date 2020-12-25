import Foundation
import utils

func determineLoopSizes(_ publicKeys: Set<UInt>) -> [UInt: UInt] {

    let subject: UInt = 7
    var val: UInt = 1

    var loopSizes = [UInt: UInt]()

    var nToFind = publicKeys.count
    var loopSize: UInt = 1
    while nToFind > 0 {
        val *= subject
        val %= 20201227

        if publicKeys.contains(val) {
            loopSizes[val] = loopSize
            nToFind -= 1
        }

        loopSize += 1
    }

    return loopSizes
}

func transformSubject(_ subject: UInt, loopSize: UInt) -> UInt {
    
    var val: UInt = 1
    
    for _ in (0..<loopSize) {
        val *= subject
        val %= 20201227
    }
    return val
}

func getPublicKeys(_ lines: [String]) -> Set<UInt> {
    return Set<UInt>(lines.map({UInt($0)!}))
}

func part1(_ lines: [String]) -> UInt {
    let publicKeys = getPublicKeys(lines)
    let loopSizeMap = determineLoopSizes(publicKeys)

    for (publicKeyA, _) in loopSizeMap {
        for (publicKeyB, loopSizeB) in loopSizeMap {
            if publicKeyA != publicKeyB {
                return transformSubject(publicKeyA, loopSize: loopSizeB)
            }
        }
    }
    return 0
}

func part2() -> String {
    return "Merry Christmas you filthy animal"
}

let filename = getFilename(day: 25)
let lines = getLines(from: filename)

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2())
