import Foundation
import utils


func parseLine(_ line: String) -> (String, UInt, String) {
    let pattern = "^(mask|mem)(\\[([0-9]+)\\])? = ([0-9X]+)$"
    let match = line.groups(for: pattern)[0]
    let (key, idx, value) = (match[1], UInt(match[3]) ?? 0, match[4])
    return (key, idx, value)
}


func part1(_ lines: [String]) -> UInt {

    var mem = [UInt: UInt]()
    var mask = String(repeating: "X", count: 36)

    func applyMask(_ mask: String, value: UInt) -> UInt {
    
        var newValue = value
        
        // Parse 1's and 0's from mask
        for (i, bit) in mask.enumerated() {
            switch String(bit) {
            case "1": // set that bit to 1 with OR
                newValue |= UInt(1) << (36 - i - 1)
            case "0": // set that bit to 0 with AND
                newValue &= ~(UInt(1) << (36 - i - 1))
            default:
                break
            }
        }
        
        return newValue
    }
    
    for line in lines {
        let (key, idx, value) = parseLine(line)

        if key == "mem" {
            mem[idx] = applyMask(mask, value: UInt(value) ?? 0)
        }
        else if key == "mask"{
            mask = value
        }
    }
    
    return mem.map({$0.1}).reduce(0,+)
}


func part2(_ lines: [String]) -> UInt {
    var mem = [UInt: UInt]()
    var mask = String(repeating: "0", count: 36)

    func applyMask(_ mask: String, address: UInt) -> Set<UInt> {

        var baseAddress = address
        
        // Pass 1, apply 1's
        for (i, bit) in mask.enumerated() {
            switch String(bit) {
            case "1": // set that bit to 1 with OR
                baseAddress |= UInt(1) << (36 - i - 1)
            default:
                break
            }
        }
        
        // Pass 2, find all combos
        var newAddresses: Set<UInt> = [baseAddress]
        
        for (i, bit) in mask.enumerated() {
            switch String(bit) {
            case "X": // set that bit to 0 with AND
                for add in newAddresses {
                    newAddresses.insert(add & ~(UInt(1) << (36 - i - 1)))
                    newAddresses.insert(add | UInt(1) << (36 - i - 1))
                }
            default:
                break
            }
        }
        
        return newAddresses
    }
    
    for line in lines {
        let (key, idx, value) = parseLine(line)

        if key == "mem" {
            let addresses = applyMask(mask, address: idx)
            for address in addresses {
                mem[address] = UInt(value) ?? 0
            }
        }
        else if key == "mask"{
            mask = value
        }
    }
    
    return mem.map({$0.1}).reduce(0,+)
}

let lines = getLines(from: getFilename(day: 14))

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))
