import Foundation
import utils

typealias Graph = [Int: [[String]]]

func buildGraph(_ lines: [String]) -> Graph {

    var graph = Graph()
    
    let pattern = "^([0-9]+): \"?([ab]|[0-9]+)\"?\\s?([0-9]+)?\\s?\\|?\\s?([0-9]+)?\\s?([0-9]+)?$"
    for line in lines {
        let matches = Array(line.groups(for: pattern)[0][1...])


        var children = [[String]]()
        let ruleNum = Int(matches[0]) ?? 0

        children.append([matches[1]])
        
        // Nested rule exists
        if Int(matches[2]) != nil {
            children[0].append(matches[2])
        }

        if Int(matches[3]) != nil {
            children.append([matches[3]])

            if Int(matches[4]) != nil {
                children[1].append(matches[4])
            }
        }

        graph[ruleNum] = children
    }

    return graph
}

func allValidMsgs(_ graph: Graph) -> [Int: [String]] {

    var memo = [Int: [String]]()
    
    func from(root: Int) -> [String] {
        // "a" or "b"
        if Int(graph[root]![0][0]) == nil {
            return graph[root]![0]
        }
        else {
            var res = [String]()
            for ls in graph[root]! { // ls = [n1, n2, ...]
                let n1 = Int(ls[0])!
                let subsubRes = memo[n1] ?? from(root: n1) // = [s1, s2, ..., sN]
                memo[n1] = subsubRes

                var subRes = [String]()
                if ls.count == 2 {
                    let n2 = Int(ls[1])!
                    let subsubRes2 = memo[n2] ?? from(root: n2)
                    memo[n2] = subsubRes2

                    for s1 in subsubRes {
                        for s2 in subsubRes2 {
                            subRes.append(s1 + s2)
                        }
                    }
                }
                else {
                    subRes = subsubRes
                }
                
                res += subRes
            }
            memo[root] = res
            return res
        }
    }

    let _ = from(root: 0)
    
    return memo
}

func part1(_ msgs: [String], _ allValids: Set<String>) -> Int {
    return msgs.filter({allValids.contains($0)}).count
}

func part2(_ msgs: [String], _ ruleMap: [Int: [String]]) -> Int {

    let set42 = Set<String>(ruleMap[42]!)
    let set31 = Set<String>(ruleMap[31]!)

    let min42 = set42.min()!.count
    let min31 = set31.min()!.count
    
    func rule8(_ msg: Substring) -> Bool {

        if set42.contains(msg) {
            return true
        }
        if msg.count < (2 * min42) {
            return false
        } 
        for idx in (min42..<(msg.count - min42 + 1)) {
            if set42.contains(msg.prefix(idx)) && rule8(msg.suffix(msg.count-idx)) {
                 return true
             }
        }
        return false
    }

    func rule11(_ msg: Substring) -> Bool {

        if msg.count < (min42 + min31) {
            return false
        }      
        for idx in (0..<(msg.count)) {
            if set42.contains(msg.prefix(idx)) && set31.contains(msg.suffix(msg.count-idx)) {
                return true
            }
        }
        if msg.count < (2 * (min42 + min31)) {
            return false
        }
        for idx1 in (min42..<(msg.count - min31)) {
            if set42.contains(msg.prefix(idx1)) {
                for idx2 in ((idx1+min42)..<(msg.count - 2)) {
                    if rule11(msg.suffix(msg.count-idx1).prefix(idx2-idx1)) && set31.contains(msg.suffix(msg.count-idx2)) {
                        return true
                    }
                }
            }
        }
        
        return false
    }
  
    func rule0(_ msg: String) -> Bool {
        
        
        if msg.count < 2 {
            return false
        }
        
        for idx in (0..<msg.count) {
            if rule8(msg.prefix(idx)) && rule11(msg.suffix(msg.count-idx)) {
                return true
            }
        }

        return false
    }

    return msgs.filter({rule0($0)}).count
}

let filename = getFilename(day: 19)
let components = getFile(filename).components(separatedBy: "\n\n")
let rules = components[0].components(separatedBy: "\n")
let msgs = components[1].components(separatedBy: "\n")

let graph = buildGraph(rules)
let ruleMap = allValidMsgs(graph)

print("Part 1")
print("--", part1(msgs, Set<String>(ruleMap[0]!)))
print("Part 2")
print("--", part2(msgs, ruleMap))
