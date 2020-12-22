import Foundation
import utils

func rotateImg(_ img: [[Int]]) -> [[Int]] {
    
    let N = img.count // 10
    
    var rotImg = img
    for i in (0..<(N/2)) {
        for j in (i..<(N-i-1)) {
            let tmp = rotImg[i][j]
            rotImg[i][j] = rotImg[N-1-j][i]
            rotImg[N-1-j][i] = rotImg[N-1-i][N-1-j]
            rotImg[N-1-i][N-1-j] = rotImg[j][N-1-i]
            rotImg[j][N-1-i] = tmp
        }
    }
    return rotImg
}   

func mirrorImg(_ img: [[Int]]) -> [[Int]] {
    let N = img.count
    var mirImg = img
    for i in (0..<N) {
        for j in (0..<(N/2)) {
            (mirImg[i][j], mirImg[i][N-j-1]) = (mirImg[i][N-j-1], mirImg[i][j])
        }
    }
    return mirImg
}

struct Tile: Hashable, Equatable {
    let num: Int
    let img: [[Int]]

    init(_ num: Int, _ img: [[Int]]) {
        self.num = num
        self.img = img
    }

    init(_ tile: String) {
        
        let components = tile.components(separatedBy: ":\n")

        let pattern = "^Tile ([0-9]+)$"
        self.num = Int(components[0].groups(for: pattern)[0][1])!

        let mapping: [Character: Character] = [".": "0", "#": "1"]
  
        let tile = components[1].components(separatedBy: "\n").map({Array($0)})      
        self.img = (0..<10).map({i in (0..<10).map({j in Int(String(mapping[tile[i][j]]!), radix: 2)!})})
    }
    
    func binToInt(_ binArray: [Int]) -> Int {
        let s = String(binArray.map({Character(String($0))}))
        return Int(s, radix: 2)!
    }
    
    func L() -> Int {
        return binToInt((0..<10).map({i in self.img[i].first!}))
    }

    func R() -> Int {
        return binToInt((0..<10).map({i in self.img[i].last!}))
    }

    func U() -> Int {
        return binToInt((0..<10).map({j in self.img.first![j]}))
    }

    func D() -> Int {
        return binToInt((0..<10).map({j in self.img.last![j]}))
    }
   
    static func == (lhs: Tile, rhs: Tile) -> Bool {
        return lhs.num == rhs.num
    }
    
    func display() {
        print("num: \(self.num), L: \(self.L()), R: \(self.R()), U: \(self.U()), D: \(self.D())")
        //print("-- img:")
        //print(self.img)
    }
    
    func rotateClockwise() -> Tile {
        return Tile(self.num, rotateImg(self.img))
    }

    // Mirrors across vertical axis
    func mirror() -> Tile {
        return Tile(self.num, mirrorImg(self.img))
    }

    func getConfigs() -> [Tile] {
        var tiles = [Tile]()

        tiles.append(self)
        for _ in (1...3) {
            tiles.append(tiles.last!.rotateClockwise())
        }

        tiles.append(self.mirror())
        for _ in (1...3) {
            tiles.append(tiles.last!.rotateClockwise())
        }

        return tiles
    }

    func getSides() -> [Int] {
        return [self.L(), self.R(), self.U(), self.D()]
    }
    
    func possibleSides() -> Set<Int> {
        let configs = self.getConfigs()
        return configs.reduce(Set<Int>(self.getSides()), {x,y in x.union(Set<Int>(y.getSides()))})
    }
}

func findCorners(_ tiles: [Tile]) -> [Tile] {

    var corners = Set<Tile>()
    
    for tileA in tiles {
        var nMatches = 0
        for tileB in tiles {
            if tileA != tileB {
                if tileA.possibleSides().intersection(tileB.possibleSides()).count > 0 {
                    nMatches += 1
                    if nMatches > 2 {
                        break
                    }
                }
            }
        }
        if nMatches == 2 {
            corners.insert(tileA)
        }
    }
    return Array(corners)
}

func print(_ tile: Tile) {
    tile.display()
}

func part1(_ corners: [Tile]) -> Int {
    return corners.map({$0.num}).reduce(1,*)
}

func part2(_ corner: Tile, _ tiles: [Tile]) -> Int {

    let N = Int(Double(tiles.count).squareRoot())
    
    func buildImage() -> [[Int]] {

        var availableTiles = [Int: Tile]()
        for tile in tiles {
            availableTiles[tile.num] = tile
        }
        availableTiles.removeValue(forKey: corner.num)
        
        var image = [[corner]]

        // build col 0, given a top left corner piece
        for i in (1..<N) {
            for (num, tile) in availableTiles {
                var foundMatch = false
                for config in tile.getConfigs() {
                    if image[i-1][0].D() == config.U() {
                        image.append([config])
                        foundMatch = true
                    }
                }
                if foundMatch {
                    availableTiles.removeValue(forKey: num)
                }
            }
            if image.count < (i+1) {
                //print("Error!")
            }
        }
        
        // build row 0, given a top left corner piece
        for j in (1..<N) {
            for (num, tile) in availableTiles {
                var foundMatch = false
                for config in tile.getConfigs() {
                    if image[0][j-1].R() == config.L() {
                        image[0].append(config)
                        availableTiles.removeValue(forKey: num)
                        foundMatch = true   
                    }
                }
                if foundMatch {
                }
            }
        }
 
        // build rows 1-11
        for i in (1..<N) {
            for j in (1..<N) {
                for (num, tile) in availableTiles {
                    var foundMatch = false
                    for config in tile.getConfigs() {
                        if image[i][j-1].R() == config.L() && image[i-1][j].D() == config.U() {
                            image[i].append(config)
                            availableTiles.removeValue(forKey: num)
                            foundMatch = true
                        }
                    }
                    if foundMatch {
                    }
                }
            }
        }

        func removeBorders(_ tile: Tile) -> [[Int]] {
            return (1...8).map({i in (1...8).map({j in tile.img[i][j]})})
        }

        var comp = [[Int]]()

        for i in (0..<N) {
            var rows = [[Int]]()
            
            for j in (0..<N) {
                // at tile [i,j]
                let minImg = removeBorders(image[i][j])

                if rows.count == 0 {
                    rows += minImg
                }
                else {
                    for k in (0..<rows.count) {
                        rows[k] += minImg[k]
                    }
                }
                
            }

            comp += rows
        }
        
        return comp
    }
    
    let img = buildImage()
    
    typealias SeaMonster = [[Int]]
                                  //1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
    let seaMonster: SeaMonster  = [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                                   [1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1],
                                   [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0]]

    var imgs = [img]
    for _ in (0..<3) {
        imgs.append(rotateImg(imgs.last!))
    }
    
    imgs.append(mirrorImg(imgs.last!))
    for _ in (0..<3) {
        imgs.append(rotateImg(imgs.last!))
    }

    func findIdxs(_ i: Int, _ j: Int) -> [(Int, Int)] {
        var idxs = [(Int, Int)]()
        
        for ii in (0..<seaMonster.count) {
            for jj in (0..<seaMonster[0].count) {
                if seaMonster[ii][jj] == 1 {
                    idxs.append((i+ii, j+jj))
                }
            }
        }
        
        return idxs
    }
    
    let targetNum: Int = (0..<seaMonster.count).map({i in seaMonster[i].reduce(0,+)}).reduce(0,+)

    func dot(_ i: Int, _ j: Int, _ img: [[Int]]) -> Int {
        var sum: Int = 0
        for ii in (0..<seaMonster.count) {
            for jj in (0..<seaMonster[0].count) {
                sum += seaMonster[ii][jj] * img[i+ii][j+jj]
            }
        }
        return sum
    }

    struct Index: Hashable {
        var i: Int
        var j: Int

        init(_ pair: (Int, Int)) {
            self.i = pair.0
            self.j = pair.1
        }
    }
    

    for img in imgs {
        var usedIdxs = Set<Index>()
        for i in (0..<(img.count - seaMonster.count + 1)) {
            for j in (0..<(img[0].count - seaMonster[0].count + 1)) {
                let d = dot(i, j, img)
                if (d == targetNum) {
                    for idxPair in findIdxs(i, j) {
                        usedIdxs.insert(Index(idxPair))
                    }
                }
            }
        }
        if usedIdxs.count > 0 {
            return img.map({row in row.reduce(0,+)}).reduce(0,+) - usedIdxs.count
        }   
    }
    return 0
}

let filename = getFilename(day: 20)
let tiles = getFile(filename).components(separatedBy: "\n\n").map({Tile($0)})
let corners = findCorners(tiles)

print("Part 1")
print("--", part1(corners))
print("Part 2")
print("--", part2(corners[3], tiles))

