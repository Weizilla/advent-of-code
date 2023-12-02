package main

import (
	"advent-of-code/solution"
	"advent-of-code/utilities"
	"fmt"
	"strings"
)

func day2Part1() any {
	inputs := solution.ReadInputStrings()
	max := map[string]int{"red": 12, "green": 13, "blue": 14}

	gameIds := 0
	for _, line := range inputs {
		s1 := strings.Split(line, ":")
		id := utilities.ToInt(strings.Split(s1[0], " ")[1])
		fmt.Println("id ", id)

		bags := strings.Split(s1[1], ";")
		fmt.Println("bags ", bags)

		valid := true
		for _, bag := range bags {
			b1 := strings.Split(bag, ",")
			fmt.Println("bag ", b1)

			for _, count := range b1 {
				c1 := strings.Split(strings.TrimSpace(count), " ")
				num := utilities.ToInt(c1[0])
				color := c1[1]

				if num > max[color] {
					valid = false
				}
			}

		}
		if valid {
			gameIds += id
		}

	}

	return gameIds
}

func day2Part2() any {
	inputs := solution.ReadInputStrings()

	gameIds := 0
	for _, line := range inputs {
		s1 := strings.Split(line, ":")
		id := utilities.ToInt(strings.Split(s1[0], " ")[1])
		fmt.Println("id ", id)

		bags := strings.Split(s1[1], ";")
		fmt.Println("bags ", bags)

		maxCount := map[string]int{"red": 0, "blue": 0, "green": 0}
		for _, bag := range bags {
			b1 := strings.Split(bag, ",")
			fmt.Println("bag ", b1)

			for _, count := range b1 {
				c1 := strings.Split(strings.TrimSpace(count), " ")
				num := utilities.ToInt(c1[0])
				color := c1[1]

				maxCount[color] = max(num, maxCount[color])

			}

		}

		power := maxCount["red"] * maxCount["blue"] * maxCount["green"]
		gameIds += power
	}

	return gameIds
}

func main() {
	fmt.Println(day2Part2())
}
