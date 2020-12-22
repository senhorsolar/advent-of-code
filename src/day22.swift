import Foundation
import utils

func parseDeck(_ deck: String) -> [Int] {
    return deck.components(separatedBy: "\n")[1...].filter({$0.count > 0}).map({Int($0)!})
}

func part1(_ deck1: [Int], _ deck2: [Int]) -> Int {
    var player1 = deck1
    var player2 = deck2

    while player1.count > 0 && player2.count > 0 {
        let (a, b) = (player1.remove(at: 0), player2.remove(at: 0))
        if a > b {
            player1 += [a, b]
        }
        else {
            player2 += [b, a]
        }
    }

    func calcScore(_ deck: [Int]) -> Int {
        return deck.reversed().enumerated().map({idx, card in (idx+1)*card}).reduce(0,+)
    }
    
    if player1.count > 0 {
        return calcScore(player1)
    }
    else {
        return calcScore(player2)
    }
}

func part2(_ deck1: [Int], _ deck2: [Int]) -> Int {

    var winnersDeck = [Int]()
    var memo = [[[Int]]: Int]()
    
    func subGame(_ deckA: [Int], _ deckB: [Int]) -> Int {
        
        var player1 = deckA
        var player2 = deckB

        var prevRounds = Set<[[Int]]>()

        while player1.count > 0 && player2.count > 0 {
            if prevRounds.contains([player1, player2]) {
                winnersDeck = player1
                return 1
            }
            else {
                prevRounds.insert([player1, player2])
                
                let (a, b) = (player1.remove(at: 0), player2.remove(at: 0))
                if player1.count >= a && player2.count >= b {

                    let round = [player1, player2]
                    let subWinner = memo[round] ?? subGame(Array(player1[..<a]), Array(player2[..<b]))
                    memo[round] = subWinner
                    
                    if subWinner == 1 {
                        player1 += [a, b]
                    }
                    else {
                        player2 += [b, a]
                    }
                }
                else {
                    if a > b {
                        player1 += [a, b]
                    }
                    else {
                        player2 += [b, a]
                    }
                }
            }
        }

        if player1.count > 0 {
            winnersDeck = player1
            return 1
        }
        else {
            winnersDeck = player2
            return 2
        }
    }
    func calcScore(_ deck: [Int]) -> Int {
        return deck.reversed().enumerated().map({idx, card in (idx+1)*card}).reduce(0,+)
    }
    let _ = subGame(deck1, deck2)
    return calcScore(winnersDeck)
}

let components = getFile(getFilename(day: 22)).components(separatedBy: "\n\n")
let deck1 = parseDeck(components[0])
let deck2 = parseDeck(components[1])


print("Part 1")
print("--", part1(deck1, deck2))
print("Part 2")
print("--", part2(deck1, deck2))
