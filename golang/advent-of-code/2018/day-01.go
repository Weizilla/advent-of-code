package main

import (
	"advent-of-code/solution"
	"fmt"
	"strconv"
)

func day1() string {
	inputs := solution.ReadInputInts(2018, 1)
	sum := 0

	for _, value := range inputs {
		sum += value
	}

	return strconv.Itoa(sum)
}

func main() {
	fmt.Println(day1())
}
