package main

import (
	"advent-of-code/solution"
	"fmt"
)

func day1Part1() any {
	inputs := solution.ReadInputInts()
	sum := 0

	for _, value := range inputs {
		sum += value
	}

	return sum
}

func day1Part2() any {
	inputs := solution.ReadInputInts()

	seen := map[int]bool{}
	sum := 0
	for {
		for _, value := range inputs {
			sum += value
			if seen[sum] {
				return sum
			} else {
				seen[sum] = true
			}
		}
	}
}

func main() {
	fmt.Println(day1Part2())
}
