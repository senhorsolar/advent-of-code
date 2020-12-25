import Foundation
import utils

func determineLoopSize(_ publicKey: UInt) -> UInt {

    let subject: UInt = 7
    var val: UInt = 1

    var loopSize: UInt = 0
    while val != publicKey {
        val = (val * subject) % 20201227
        loopSize += 1
    }

    return loopSize
}

func getPublicKeys(_ lines: [String]) -> [UInt] {
    return lines.map({UInt($0)!})
}

func part1(_ lines: [String]) -> UInt {
    let publicKeys = getPublicKeys(lines)

    let publicKeyDoor = publicKeys[1]
    let loopSizeCard = determineLoopSize(publicKeys[0])

    var val: UInt = 1
    
    for _ in (0..<loopSizeCard) {
        val = (val * publicKeyDoor) % 20201227
    }
    
    return val
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
