import Foundation
import utils


func parseKeyVals(_ cand: String) -> [String: String] {
    
    var keyVals: [String: String] = [:]

    // Separate by '\n' or ' '
    let pairs = cand.components(separatedBy: .whitespacesAndNewlines)
    
    for pair in pairs {
        let contents = pair.split(separator: ":")

        if contents.count >= 2 {
            keyVals[String(contents[0])] = String(contents[1])
        }  
    }

    return keyVals
}

// Part 1
// Check if passport

let necessaryKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

func part1(_ cand: String) -> Bool {
    let keyVals = parseKeyVals(cand)
    return (necessaryKeys.map {keyVals[$0] != nil}).allSatisfy({$0})
}

// Part 2
// Check if valid passport

func byrRule(byr: String) -> Bool {
    let byrInt = Int(byr) ?? 0
    return (byr.count == 4) && (byrInt >= 1920) && (byrInt <= 2002)
}

func iyrRule(iyr: String) -> Bool {
    let iyrInt = Int(iyr) ?? 0
    return (iyr.count == 4) && (iyrInt >= 2010) && (iyrInt <= 2020)
}

func eyrRule(eyr: String) -> Bool {
    let eyrInt = Int(eyr) ?? 0
    return (eyr.count == 4) && (eyrInt >= 2020) && (eyrInt <= 2030)
}

func hgtRule(hgt: String) -> Bool {
    let pattern = "([0-9]+)(cm|in)"

    let matches = hgt.groups(for: pattern)

    if matches.count > 0 {
        let match = matches[0]

        let height = Int(match[1]) ?? 0
        let unit = match[2]

        switch unit {
        case "cm":
            return (height >= 150) && (height <= 193)
        case "in":
            return (height >= 59) && (height <= 76)
        default:
            break
        }
    }
    return false
}

func hclRule(hcl: String) -> Bool {
    let pattern = "#[0-9a-f]{6}"
    let matches = hcl.groups(for: pattern)
    return matches.count > 0
}

func eclRule(ecl: String) -> Bool {
    let pattern = "^(amb|blu|brn|gry|grn|hzl|oth)$"
    let matches = ecl.groups(for: pattern)
    return matches.count > 0 
}

func pidRule(pid: String) -> Bool {
    let pattern = "^[0-9]{9}$"
    let matches = pid.groups(for: pattern)
    return matches.count > 0
}

var keyRuleMap = [
  "byr": byrRule,
  "iyr": iyrRule,
  "eyr": eyrRule,
  "hgt": hgtRule,
  "hcl": hclRule,
  "ecl": eclRule,
  "pid": pidRule
]

func part2(_ cand: String) -> Bool {

    let keyVals = parseKeyVals(cand)

    let fieldChecks = necessaryKeys.map { (key: String) -> Bool in
        if let ruleFunc = keyRuleMap[key] {
            if let val = keyVals[key] {
                return ruleFunc(val)
            }
            return false
        }
        else {
            return false
        }
    }

    return fieldChecks.allSatisfy({$0})
}

let file = getFile(getFilename(day: 4))
let passportCandidates = file.components(separatedBy: "\n\n")

let nPresentPassports = (passportCandidates.map {Int(part1($0))}).reduce(0,+)
let nValidPassports = ((passportCandidates.filter { part1($0)}).map { Int(part2($0))}).reduce(0,+)

print("Part 1")
print("--\(nPresentPassports)")

print("Part 2")
print("--\(nValidPassports)")
