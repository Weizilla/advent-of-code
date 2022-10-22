package main

import (
	"advent-of-code/solution"
	"advent-of-code/utilities"
	"fmt"
	"regexp"
)

type claim struct {
	id     int
	x      int
	y      int
	width  int
	height int
}

type point struct {
	x int
	y int
}

func (c *claim) String() string {
	return fmt.Sprintf("#%d @ (%d),(%d): (%d)x(%d)", c.id, c.x, c.y, c.width, c.height)
}

func day3Part1() any {
	var claims = readClaims()
	var board = map[point]int{}

	for _, claim := range claims {
		for x := claim.x; x < claim.x+claim.width; x++ {
			for y := claim.y; y < claim.y+claim.height; y++ {
				board[point{x, y}] = board[point{x, y}] + 1
			}
		}
	}

	overlaps := 0
	for _, value := range board {
		if value >= 2 {
			overlaps += 1
		}
	}

	return overlaps
}

func readClaims() []claim {
	inputs := solution.ReadInputStrings()
	inputPattern, _ := regexp.Compile("#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)")

	var claims []claim

	for _, input := range inputs {
		parts := inputPattern.FindStringSubmatch(input)

		claims = append(claims, claim{
			utilities.ToInt(parts[1]),
			utilities.ToInt(parts[2]),
			utilities.ToInt(parts[3]),
			utilities.ToInt(parts[4]),
			utilities.ToInt(parts[5]),
		})
	}

	return claims
}

type count struct {
	overlap int
	ids     []int
}

func day3Part2() any {
	var claims = readClaims()
	var board = map[point]*count{}

	allIds := map[int]bool{}
	for _, claim := range claims {
		allIds[claim.id] = true

		for x := claim.x; x < claim.x+claim.width; x++ {
			for y := claim.y; y < claim.y+claim.height; y++ {
				if value, ok := board[point{x, y}]; ok {
					value.overlap += 1
					value.ids = append(value.ids, claim.id)
				} else {
					ids := []int{claim.id}
					board[point{x, y}] = &count{1, ids}
				}
			}
		}
	}

	for _, count := range board {
		if count.overlap > 1 {
			for _, id := range count.ids {
				delete(allIds, id)
			}
		}
	}

	return utilities.Keys(allIds)[0]
}

func main() {
	fmt.Println(day3Part2())
}
