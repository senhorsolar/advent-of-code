import Foundation
import utils


func cleanLine(_ line: String) -> String {
    // Drop trailing period, and replace bags with bag
    return line.dropLast().replacingOccurrences(of: "bags", with: "bag", options: .regularExpression)
}

typealias Vertex = (node: String, neighbors: [String: Int])

func parseLine(_ line: String) -> Vertex {
    // Separate lines by ' contain '
    let bags = cleanLine(line).components(separatedBy: " contain ")
    let node = bags[0]

    // neighbors with name: weight map
    var neighbors = [String: Int]()

    // do nothing
    if bags[1] == "no other bag" {
        
    }
    // Extract weights from neighbors
    else {
        
        let pattern = "^([0-9]) ([a-zA-Z ]* bag)$"
        let neighborsRaw = bags[1].components(separatedBy: ", ")

        for neighbor in neighborsRaw {
            let matches = neighbor.groups(for: pattern)[0]
            let weight = Int(matches[1]) ?? 0
            let bagType = matches[2]

            neighbors[bagType] = weight
        }
    }

    return (node: node, neighbors: neighbors)
}

typealias Graph = [String: [String: Int]]

func buildGraph(_ vertices: [Vertex]) -> Graph  {
    var graph = [String: [String: Int]]()

    for vertex in vertices {
        graph[vertex.node] = vertex.neighbors
    }
    
    return graph
}

func countContainGold(_ graph: Graph) -> Int {
    
    var visited = [String: Bool]()

    func helper(_ node: String) -> Bool {
        
        if let neighbors = graph[node] {

            if let res = visited[node] {
                return res
            }
            
            if neighbors["shiny gold bag"] != nil {
                visited[node] = true
                return true
            }
            else {
                let answers = neighbors.keys.map {helper($0)}
                let answer = answers.contains {$0}
                visited[node] = answer
                return answer
            }
            
        }
        return false
    }

    return ((graph.keys.map {helper($0)}).map {Int($0)}).reduce(0, +)
}

func countInBag(_ node: String, graph: Graph) -> Int {

    var memo = [String: Int]()

    func helper(_ bag: String) -> Int {
        
        if let res = memo[bag] {
            return res
        }

        var count = 0

        if let neighbors = graph[bag] {
            for (neighbor, weight) in neighbors {
                count += weight * (1 + helper(neighbor))
            }
        }
        memo[bag] = count
        return count
    }

    return helper(node)
}

let lines = getLines(from: getFilename(day: 7))
let vertices = lines.map {parseLine($0)}
let graph = buildGraph(vertices)

print("Part 1")
print("--", countContainGold(graph))
print("Part 2")
print("--", countInBag("shiny gold bag", graph: graph))

