import Foundation
import utils

class Node: Equatable {
    let value: Int
    var next: Node?

    init(_ value: Int) {
        self.value = value
    }

    init(_ value: Int, next: Node) {
        self.value = value
        self.next = next
    }

    static func == (lhs: Node, rhs: Node) -> Bool {
        return lhs.value == rhs.value
    }
}

func createLL(_ input: [Int]) -> Node {

    let firstNode = Node(input.first!)

    var node = firstNode
    for val in input[1..<input.count] {
        node.next = Node(val)
        node = node.next!
    }
    node.next = firstNode
    return firstNode
}

func play(_ input: [Int], nMoves: Int, nAfterOne: Int) -> [Int] {
    
    var current = createLL(input)

    // create mapping from vals to nodes for easy access
    var nodeMap = [current.value: current]
    var it = current.next!
    while it != current {
        nodeMap[it.value] = it
        it = it.next!
    }

    let maxVal = input.max()!
    
    for _ in (0..<nMoves) {
        
        // remove three next cups
        let threeCups = [current.next!, current.next!.next!, current.next!.next!.next!]
        current.next = threeCups.last!.next!

        // select destination cup
        var destVal = (current.value == 1) ? maxVal : current.value - 1
        while threeCups.contains(where: {$0.value == destVal}) {
            destVal = (destVal == 1) ? maxVal : destVal - 1
        }

        // find dest
        var dest = nodeMap[destVal]!        

        // add three cups right after dest
        threeCups.last!.next = dest.next!
        dest.next = threeCups.first!

        current = current.next!
    }

    let firstNode = nodeMap[1]!
    
    var node = firstNode.next!
    var res = [Int]()
    for _ in (0..<nAfterOne) {
        res.append(node.value)
        node = node.next!
    }
    
    return res
}

func part1(_ input: [Int]) -> String {
    let res = play(input, nMoves: 100, nAfterOne: 8)
    return res.map({String($0)}).joined(separator: "")
}

func part2(_ input: [Int]) -> UInt {
    var fixedInput = input
    var val = 10
    while val <= 1000000 {
        fixedInput.append(val)
        val += 1
    }

    let res = play(fixedInput, nMoves: 10000000, nAfterOne: 2)
    return UInt(res[0]) * UInt(res[1])
}

let input = getFile(getFilename(day: 23)).replacingOccurrences(of: "\n", with: "").map({Int($0)})

print("Part 1")
print("--", part1(input))
print("Part 2")
print("--", part2(input))
