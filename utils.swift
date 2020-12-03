import Foundation

public func getFilename(day: Int, directory: String = "data") -> String {
    let filename = String(format: "%@/day%d-input.txt", directory, day)
    return filename
}

// Loads lines from a given filename
public func getLines(from filename: String) -> Array<String> {
    return try! String(contentsOfFile: filename).split(separator: "\n").map {String($0)}
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


