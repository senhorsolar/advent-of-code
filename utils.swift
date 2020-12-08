import Foundation

public func getFilename(day: Int, directory: String = "data") -> String {
    let filename = String(format: "%@/day%d.txt", directory, day)
    return filename
}

public func getFile(_ filename: String) -> String {
    return try! String(contentsOfFile: filename)
}

// Loads lines from a given filename
public func getLines(from filename: String) -> Array<String> {
    return getFile(filename).split(separator: "\n").map {String($0)}
}


// Function for generating k-combinations
public func kcombinations<T>(_ arr: [T], k: Int) -> [[T]] {
    return kcombinations(arr[...], k: k)
}

public func kcombinations<T>(_ arr: ArraySlice<T>, k: Int) -> [[T]] {
    if (k > arr.count) || (arr.count == 0) {
        return [[T]]()
    }
    
    if k == 1 {
        return arr.map {[$0]}
    }

    if k == arr.count {
        return [Array(arr)]
    }

    let upperIdx: Int = (arr.count - 1)
    let startIdx: Int = arr.startIndex

    func foo(_ idx: Int) -> [[T]] {
        return kcombinations(arr[(startIdx+idx+1)...], k: k - 1).map {[arr[startIdx+idx]] + $0}
    }
    return (0..<upperIdx).flatMap { idx in
        return foo(idx)
    }
}

// Function for extracting groups from regex pattern
// https://stackoverflow.com/a/53652037
extension String {
    public func groups(for regexPattern: String) -> [[String]] {
        do {
            let text = self
            let regex = try NSRegularExpression(pattern: regexPattern)
            let matches = regex.matches(in: text,
                                        range: NSRange(text.startIndex..., in: text))
            return matches.map { match in
                return (0..<match.numberOfRanges).map {
                    let rangeBounds = match.range(at: $0)
                    guard let range = Range(rangeBounds, in: text) else {
                        return ""
                    }
                    return String(text[range])
                }
            }
        } catch let error {
            print("invalid regex: \(error.localizedDescription)")
            return []
        }
    }
}

// XOR for Bool
extension Bool {
    public static func ^ (left: Bool, right: Bool) -> Bool {
        return left != right
    }
}

// Int(Bool)
extension Int {
    public init(_ bool: Bool) {
        self = bool ? 1 : 0
    }   
}

public func powInt(_ base: Int, _ power: Int) -> Int {
    if power == 0 {
        return 1
    }

    if (power % 2) == 0 {
        let res = powInt(base, power / 2)
        return res * res
    }
    else {
        let res = powInt(base, power - 1)
        return base * res
    }
}
