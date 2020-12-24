import Foundation
import utils

enum Direction: CaseIterable {
    case west
    case northwest
    case southwest
    case east
    case northeast
    case southeast
}

struct HexPoint: Hashable, Equatable {
    let x: Int
    let y: Int
    let z: Int

    init() {
        self.x = 0
        self.y = 0
        self.z = 0
    }
    
    init(_ direction: Direction) {
        switch direction {
        case .west:
            self.init(-1, 1, 0)
        case .northwest:
            self.init(0, 1, -1)
        case .southwest:
            self.init(-1, 0, 1)
        case .east:
            self.init(1, -1, 0)
        case .northeast:
            self.init(1, 0, -1)
        case .southeast:
            self.init(0, -1, 1)
        }
    }
    
    init(_ x: Int, _ y: Int, _ z: Int) {
        self.x = x
        self.y = y
        self.z = z
    }

    static func == (lhs: HexPoint, rhs: HexPoint) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z
    }

    static func + (lhs: HexPoint, rhs: HexPoint) -> HexPoint {
        return HexPoint(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z)
    }
}

func parseLine(_ line: String) -> [Direction] {
    
    var directions = [String]()

    let mapping: [String: Direction] = ["nw": .northwest,
                                        "w": .west,
                                        "sw": .southwest,
                                        "ne": .northeast,
                                        "e": .east,
                                        "se": .southeast]

    let chars = Array(line)
    var idx = 0
    while idx < chars.count {
        switch chars[idx] {
        case "n", "s":
            directions.append(String(chars[idx...(idx+1)]))
            idx += 2
        default:
            directions.append(String(chars[idx]))
            idx += 1
        }
    }
    return directions.map({mapping[$0]!})
}

func flipTile(_ line: String) -> HexPoint {
        return parseLine(line).reduce(HexPoint(), {x, dir in x + HexPoint(dir)})
}

func part1(_ lines: [String]) -> Int {

    var tiles = [HexPoint: Int]()

    for line in lines {
        let tile = flipTile(line)
        if tiles[tile] == nil {
            tiles[tile] = 1
        }
        else {
            tiles[tile]! += 1
        }
    }
    
    return tiles.values.reduce(0, {x, y in x + Int(y % 2 == 1)})
}

func part2(_ lines: [String]) -> Int {

    enum Color {
        case white
        case black
    }

    typealias Grid = [HexPoint: Color]
    
    func initGrid() -> Grid {
        var grid = Grid()
        for line in lines {
            let tile = flipTile(line)
            if let color = grid[tile] {
                grid[tile] = (color == .black) ? .white : .black
            }
            else {
                grid[tile] = .black
            }
        }
        return grid
    }
    
    func expandGrid(_ grid: inout Grid) {
        for point in grid.keys {
            for direction in Direction.allCases {
                let otherPoint = point + HexPoint(direction)
                if grid[otherPoint] == nil {
                    grid[otherPoint] = .white
                }
            }
        }
    }

    func applyRules(_ grid: Grid) -> Grid {
        var nextGrid = grid

        for (point, color) in grid {

            var nAdjacentBlacks = 0
            for direction in Direction.allCases {
                let adjPoint = point + HexPoint(direction)
                if let c = grid[adjPoint] {
                    nAdjacentBlacks += Int(c == .black)
                }
            }
                
            switch color {
            case .black:
                if nAdjacentBlacks == 0 || nAdjacentBlacks > 2 {
                    nextGrid[point] = .white
                }
            case .white:
                if nAdjacentBlacks == 2 {
                    nextGrid[point] = .black
                }
            }
        }
        return nextGrid
    }


    var grid = initGrid()
    for _ in (0..<100) {
        expandGrid(&grid)
        grid = applyRules(grid)
    }
    
    return grid.reduce(0, {x, y in x + Int(y.value == .black)})
}

let filename = getFilename(day: 24)
let lines = getLines(from: filename)


print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))
