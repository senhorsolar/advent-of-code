import Foundation
import utils

typealias Ingredient = String
typealias Allergen = String

func parseLine(_ line: String) -> ([Ingredient], [Allergen]) {
    let pattern = "^([a-z ]+) \\(contains ([a-z, ]+)\\)$"
    let matches = line.groups(for: pattern)[0]

    let ingredients = matches[1].components(separatedBy: " ")
    let allergens = matches[2].components(separatedBy: ", ")

    return (ingredients, allergens)
}

func getAllergenMap(_ lines: [String]) -> [Allergen: Set<Ingredient>] {
    var allergenMap = [Allergen: Set<Ingredient>]()

    for (ingredients, allergens) in lines.map({line in parseLine(line)}) {
        for allergen in allergens {
            if let val = allergenMap[allergen] {
                allergenMap[allergen] = val.intersection(ingredients)
            }
            else {
                allergenMap[allergen] = Set<Ingredient>(ingredients)
            }
        }
    }
    return allergenMap
}

func part1(_ lines: [String]) -> Int {

    var allIngredients = Set<Ingredient>()
    let allergenMap = getAllergenMap(lines)

    for (ingredients, _) in lines.map({line in parseLine(line)}) {
        allIngredients = allIngredients.union(ingredients)
    }

    var possibleIngredients = Set<Ingredient>()
    for ingredients in allergenMap.values {
        possibleIngredients = possibleIngredients.union(ingredients)
    }

    let nonPossibleIngredients = allIngredients.subtracting(possibleIngredients)
    var nAppears = 0

    for (ingredients, _) in lines.map({line in parseLine(line)}) {
        for ingredient in ingredients {
            nAppears +=  Int(nonPossibleIngredients.contains(ingredient)) 
        }
    }
    return nAppears
}

func part2(_ lines: [String]) -> [String] {

    var allergenMap = getAllergenMap(lines)

    func matchingFound(_ allergenMap: [Allergen: Set<Ingredient>]) -> Bool {
        return allergenMap.allSatisfy({$0.value.count < 1})
    }
    
    var matches = [Allergen: Ingredient]()
    
    while !matchingFound(allergenMap) {
        var removedIngredient = ""
        for (allergen, ingredients) in allergenMap {
            if ingredients.count == 1 {
                removedIngredient = ingredients.first!
                matches[allergen] = removedIngredient
            }
        }

        allergenMap = Dictionary(uniqueKeysWithValues: allergenMap.map({($0.key, $0.value.subtracting([removedIngredient]))}))
    }

    return matches.map({($0.key, $0.value)}).sorted(by: {$0.0 < $1.0}).map({$0.1})
}

let filename = getFilename(day: 21)
let lines = getLines(from: filename)

print("Part 1")
print("--", part1(lines))
print("Part 2")
print("--", part2(lines))

