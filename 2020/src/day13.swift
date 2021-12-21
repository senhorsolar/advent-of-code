import Foundation
import utils


func part1(_ time: Int, ids: [Int]) -> Int {

    let validIds = ids.filter({$0 > 0})
    
    let factors = validIds.map({Int(ceil(Double(time) / Double($0)))})
    let bestID = validIds[factors.argmin()!]
    return bestID * (bestID * factors.min()! - time)
}


func part2(_ rawIds: [Int]) -> Int {

    let idPairs = rawIds.enumerated().filter({$1 > 0})

    //  time + idx_1 = 0 mod n_1 => time = -idk_1 mod n_1
    //            .....
    //  time + idx_k = 0 mod n_k => time = -idx_k mod n_k
    //
    // Can use the Chinese remainder theorem
    // a_i = -idx_i
    // n_i = id_i
    
    let a = idPairs.map({-$0.0})
    let n = idPairs.map({$0.1})

    // https://rosettacode.org/wiki/Chinese_remainder_theorem#Swift
    func crt(_ a: [Int], _ n: [Int]) -> Int {
        
        func euclid(_ m: Int, _ n: Int) -> (Int, Int) {
            if m % n == 0 {
                return (0, 1)
            }
            else {
                let (r, s) = euclid(n % m, m)
                return (s - r * (n / m), r)
            }
        }

        let N = n.reduce(1, *)
        
        var x = 0
        for i in 0..<a.count {
            let (_, s) = euclid(n[i], N / n[i])
            x += a[i] * s * (N / n[i])
        }
        
        return x % N
    }
    
    return crt(a, n)
}

let lines = getLines(from: getFilename(day: 13))

let earliestTime = Int(lines[0]) ?? 0
let ids = lines[1].split(separator: ",").map({Int($0) ?? 0})

print("Part 1")
print("--", part1(earliestTime, ids: ids))
print("Part 2")
print("--", part2(ids))

