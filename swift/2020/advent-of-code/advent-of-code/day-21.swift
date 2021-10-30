import Foundation

func day21Part1() -> Int {
    let lines = readInput(21)
    let foods = lines.enumerated()
        .map({(id, line) in parseLineDay21(id, line)})
        .reduce(into: [Int: Food](), {$0[$1.id] = $1})

    let (allergenToFood, ingredientToFood) = mapFoods(foods: foods)

    print("a2f \(allergenToFood)")
    print("i2f \(ingredientToFood)")

    var allergenIngredients: Set<String> = []
    for (allergen, allergenFoodIds) in allergenToFood {
        for (ingredient, ingredientFoodIds) in ingredientToFood {
            if (allergenFoodIds.subtracting(ingredientFoodIds).isEmpty) {
                allergenIngredients.insert(ingredient)
            }
        }
    }

    print("allergen ingredients \(allergenIngredients)")

    var numFoods = 0
    for ingredient in ingredientToFood.keys {
        if !allergenIngredients.contains(ingredient) {
            numFoods += ingredientToFood[ingredient]!.count
        }
    }

    return numFoods
}


func day21Part2() -> Int {
    let lines = readInput(21)
    let foods = lines.enumerated()
        .map({(id, line) in parseLineDay21(id, line)})
        .reduce(into: [Int: Food](), {$0[$1.id] = $1})

    let (allergenToFood, ingredientToFood) = mapFoods(foods: foods)

    print("a2f \(allergenToFood)")
    print("i2f \(ingredientToFood)")

    var numFoodPerAllergen: [(Int, String)] = []
    for (allergen, allergenFoodIds) in allergenToFood {
        numFoodPerAllergen.append((allergenFoodIds.count, allergen))
    }

    numFoodPerAllergen.sort(by: {(a: (Int, String), b: (Int, String))
        in a.0 == b.0 ? a.1 < b.1 : a.0 > b.0})

    print("num \(numFoodPerAllergen)")

    var allergenIngredients: [String: Set<String>] = [:]

    for (_, allergen) in numFoodPerAllergen {
        let allergenFoodIds = allergenToFood[allergen]!
        for (ingredient, ingredientFoodIds) in ingredientToFood {
            print("allergen=\(allergen) \(allergenFoodIds) ingredient=\(ingredient) \(ingredientFoodIds)")
            if (allergenFoodIds.subtracting(ingredientFoodIds).isEmpty) {
                print("setting \(allergen) to \(ingredient)")
                allergenIngredients.merge([allergen: [ingredient]],
                    uniquingKeysWith: {$0.union($1)})
            }
        }
    }

    print("allergen ingredients \(allergenIngredients)")

    while (!allSingles(&allergenIngredients)) {
        let singles = allergenIngredients.filter({(a, i) in i.count == 1}).flatMap({(a, i) in i})
        print("singles \(singles)")
        for (allergen, ingredients) in allergenIngredients {
            if ingredients.count > 1 {
                allergenIngredients[allergen] = ingredients.subtracting(singles)
            }
        }
        print("allergen ingredients \(allergenIngredients)")
    }

    var sortedAllergens = Array(allergenIngredients.keys)
    sortedAllergens.sort()

    var sortedIngredients: [String] = []
    for allergen in sortedAllergens {
        sortedIngredients.append(allergenIngredients[allergen]!.first!)
    }

    print("answer: \(sortedIngredients.joined(separator: ","))")

    return 0
}

func allSingles(_ allergenIngredients: inout [String: Set<String>]) -> Bool {
    allergenIngredients.values.filter({$0.count > 1}).count == 0
}


private func mapFoods(foods: [Int: Food]) -> ([String: Set<Int>], [String: Set<Int>]) {
    var allergenToFood: [String: Set<Int>] = [:]
    var ingredientToFood: [String: Set<Int>] = [:]

    for (id, food) in foods {
        for allergen in food.allergens {
            allergenToFood.merge([allergen: Set([food.id])],
                uniquingKeysWith: {(a: Set<Int>, b: Set<Int>) in a.union(b)})
        }
        for ingredient in food.ingredients {
            ingredientToFood.merge([ingredient: Set([food.id])],
                uniquingKeysWith: {(a: Set<Int>, b: Set<Int>) in a.union(b)})
        }
    }

    return (allergenToFood, ingredientToFood)
}

func parseLineDay21(_ id: Int, _ line: String) -> Food {
    let splits = line
        .replacingOccurrences(of: ",", with: "")
        .replacingOccurrences(of: ")", with: "")
        .components(separatedBy: " (contains ")
    let ingredients = Set(splits[0].components(separatedBy: " "))
    let allergens = Set(splits[1].components(separatedBy: " "))
    return Food(id: id, ingredients: ingredients, allergens: allergens)
}


class Food: CustomStringConvertible {
    let id: Int
    let ingredients: Set<String>
    let allergens: Set<String>

    init(id: Int, ingredients: Set<String>, allergens: Set<String>) {
        self.id = id
        self.ingredients = ingredients
        self.allergens = allergens
    }

    var description: String {
        "\(ingredients) \(allergens)"
    }
}
