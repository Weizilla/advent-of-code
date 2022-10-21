package main

import (
	"advent-of-code/solution"
	"fmt"
)

func day2Part1() any {
	inputs := solution.ReadInputStrings()

	count2 := 0
	count3 := 0

	for _, input := range inputs {
		occ := countOcc(input)

		has2 := false
		has3 := false
		for _, v := range occ {
			if v == 2 {
				has2 = true
			} else if v == 3 {
				has3 = true
			}
		}

		if has2 {
			count2++
		}

		if has3 {
			count3++
		}
	}
	return count2 * count3
}

func countOcc(input string) map[rune]int {
	occ := map[rune]int{}

	for _, c := range input {
		occ[c] += 1
	}

	return occ
}

func day2Part2() any {
	inputs := solution.ReadInputStrings()

	allPossible := map[valueWithRemoved]int{}

	for _, input := range inputs {
		p := genPossibles(input)
		for _, p2 := range p {
			allPossible[p2] += 1
		}
	}

	for k, v := range allPossible {
		if v == 2 {
			return k.value
		}
	}

	panic("not found")
}

func genPossibles(input string) []valueWithRemoved {
	results := make([]valueWithRemoved, 0)
	for i := range input {
		newStr := input[:i] + input[i+1:]
		results = append(results, valueWithRemoved{newStr, i})
	}
	return results
}

type valueWithRemoved struct {
	value   string
	removed int
}

func main() {
	fmt.Println(day2Part2())
}
