import Foundation
import utils

enum Cube {
    case active, inactive
}

struct Point: Hashable {
    let i: Int
    let j: Int
    let k: Int
    let q: Int

    init(_ i: Int, _ j: Int, _ k: Int) {
        self.i = i
        self.j = j
        self.k = k
        self.q = 0
    }

    init(_ i: Int, _ j: Int, _ k: Int, _ q: Int) {
        self.i = i
        self.j = j
        self.k = k
        self.q = q
    }
}

typealias Grid = [Point: Cube]

func initGrid(_ lines: [String]) -> Grid {

    var grid = Grid()
    let k = 0
    
    for (i, line) in lines.enumerated() {
        for (j, cube) in line.enumerated() {
            
            let point = Point(i, j, k)
            
            switch String(cube) {
            case ".":
                grid[point] = .inactive
            case "#":
                grid[point] = .active
            default:
                break
            }
        }
    }

    return grid
}


func getNeighbors(_ grid: Grid, _ point: Point, part2: Bool = false) -> [Point] {
    
    var points: [Point] = []
    
    for di in [-1, 0, 1] {
        for dj in [-1, 0, 1] {
            for dk in [-1, 0, 1] {
                if !part2 {
                    if (di, dj, dk) != (0, 0, 0) {
                        points.append(Point(point.i + di, point.j + dj, point.k + dk))           
                    }
                }
                else {
                    for dq in [-1, 0, 1] {
                        if (di, dj, dk, dq) != (0, 0, 0, 0) {
                            points.append(Point(point.i+di, point.j+dj, point.k+dk, point.q+dq))
                        }
                    }
                }
            }
        }
    }
    return points
}

func expandGrid(_ grid: inout Grid, part2: Bool = false) {
    for (point, _) in grid {
        for neighbor in getNeighbors(grid, point, part2: part2) {
            if grid[neighbor] == nil {
                grid[neighbor] = .inactive
            }
        }
    }
}

func countActive(_ grid: Grid, _ point: Point, part2: Bool = false) -> Int {
    let neighbors = getNeighbors(grid, point, part2: part2)
    
    var nActive = 0
    
    for neighbor in neighbors {
        if let cube = grid[neighbor] {
            nActive += Int(cube == .active)
        }
    }
    
    return nActive
}

func applyRules(_ grid: inout Grid, part2: Bool = false) {
    
    var changes = Grid()
    
    for (point, cube) in grid {
        let nActive = countActive(grid, point, part2: part2)
        
        switch cube {
        case .active:
            if nActive == 2 || nActive == 3 {
                // no change
            }
            else {
                changes[point] = .inactive
                //grid[point] = .inactive
            }
        case .inactive:
            if nActive == 3 {
                changes[point] = .active
                //grid[point] = .active
            }
            else {
                // no change
            }
        }
    }
    
    for (point, cube) in changes {
        grid[point] = cube
    }
}

func part1(_ lines: [String]) -> Int {

    var grid = initGrid(lines)
    for _ in 0..<6 {
        expandGrid(&grid)
        applyRules(&grid)   
    }
    
    return grid.filter({$0.1 == .active}).count
}

func part2(_ lines: [String]) -> Int {
    
    var grid = initGrid(lines)
    for _ in 0..<6 {
        expandGrid(&grid, part2: true)
        applyRules(&grid, part2: true)   
    }
    
    return grid.filter({$0.1 == .active}).count
}

let lines = getLines(from: getFilename(day: 17))

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))
