import Foundation
import utils


func parseLine(_ line: String) -> (String, Int) {
    let pattern = "^([a-zA-Z])([0-9]*)$"
    let match = line.groups(for: pattern)[0]

    let action = match[1]
    let value = Int(match[2]) ?? 0
    return (action, value)
}

func part1(_ lines: [String]) -> Int {

    var deg = 0, east = 0, north = 0
    
    let degToDirection = [0: "E", 90: "N", 180: "W", 270: "S"]
    
    for line in lines {
        var (action, value) = parseLine(line)

        if action == "F" {
            action = degToDirection[deg]!
        }

        if action == "E" {
            east += value
        }
        else if action == "W" {
            east -= value
        }
        else if action == "N" {
            north += value
        }
        else if action == "S" {
            north -= value
        }
        else if action == "L" {
            //print("value: \(value)")
            deg += value
        }
        else if action == "R" {
            //print("value: \(value)")
            deg -= value
        }

        deg %= 360
        if deg < 0 {
            deg += 360
        }
    }
    
    return abs(east) + abs(north)
}

func part2(_ lines: [String]) -> Int {

    var east = 0, north = 0
    var wpeast = 10, wpnorth = 1
       
    for line in lines {
        let (action, value) = parseLine(line)

        if action == "F" {
            east += wpeast * value
            north += wpnorth * value
        }

        if action == "E" {
            wpeast += value
        }
        else if action == "W" {
            wpeast -= value
        }
        else if action == "N" {
            wpnorth += value
        }
        else if action == "S" {
            wpnorth -= value
        }
        else if action == "L" {
            let nturns = value / 90
            for _ in 0..<nturns {
                (wpeast, wpnorth) = (-wpnorth, wpeast)
            }
        }
        else if action == "R" {
            let nturns = value / 90
            for _ in 0..<nturns {
                (wpeast, wpnorth) = (wpnorth, -wpeast)
            }
        }
    }
    
    return abs(east) + abs(north)
}


let lines = getLines(from: getFilename(day: 12))

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))


