package main

import (
	"advent-of-code/solution"
	"fmt"
)

const year = 2018
const day = 1

func day1() any {
	inputs := solution.ReadInputInts(year, day)
	sum := 0

	for _, value := range inputs {
		sum += value
	}

	return sum
}

func day2() any {
	inputs := solution.ReadInputInts(year, day)

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
	fmt.Println(day2())
}
