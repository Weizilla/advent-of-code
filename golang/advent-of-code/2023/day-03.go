package main

import (
	"advent-of-code/solution"
	"advent-of-code/utilities"
	"fmt"
	"strconv"
)

var notSymbols = map[string]struct{}{
	"1": struct{}{},
	"2": struct{}{},
	"3": struct{}{},
	"4": struct{}{},
	"5": struct{}{},
	"6": struct{}{},
	"7": struct{}{},
	"8": struct{}{},
	"9": struct{}{},
	"0": struct{}{},
	".": struct{}{},
}

func day3Part1() any {
	input := solution.ReadInputStrings()

	allDigits := []int{}
	for y := 0; y < len(input); y++ {
		digits := ""
		isPart := false
		for x := 0; x < len(input[y]); x++ {
			curr := string(input[y][x])
			_, err := strconv.Atoi(curr)
			if err != nil {
				if d, err := strconv.Atoi(digits); err == nil && isPart {
					allDigits = append(allDigits, d)
				}
				digits = ""
				isPart = false
			} else {
				digits += curr
				isPart = isPart || isPartCheck(x, y, input)
			}
		}

		if d, err := strconv.Atoi(digits); err == nil && isPart {
			allDigits = append(allDigits, d)
		}
		digits = ""
		isPart = false
	}

	return utilities.Sum(allDigits)
}

func isPartCheck(x int, y int, board []string) bool {
	for yy := -1; yy < 2; yy++ {
		cY := y + yy

		if cY < 0 || cY >= len(board) {
			continue
		}

		for xx := -1; xx < 2; xx++ {
			cX := x + xx

			if cX < 0 || cX >= len(board[cY]) {
				continue
			}

			c := board[cY][cX]

			_, ok := notSymbols[string(c)]

			if !ok {
				return true
			}
		}
	}

	return false
}

type Coord struct {
	x int
	y int
}

func day3Part2() any {
	input := solution.ReadInputStrings()

	gears := map[Coord][]int{}

	for y := 0; y < len(input); y++ {
		digits := ""
		isGear := false
		var gearCoord Coord
		for x := 0; x < len(input[y]); x++ {
			curr := string(input[y][x])
			_, err := strconv.Atoi(curr)
			if err != nil {
				if d, err := strconv.Atoi(digits); err == nil && isGear {
					gears[gearCoord] = append(gears[gearCoord], d)
				}
				digits = ""
				isGear = false
			} else {
				digits += curr
				cX, cY, isCurrGear := isGearCheck(x, y, input)
				if isCurrGear {
					gearCoord = Coord{cX, cY}
				}
				isGear = isGear || isCurrGear
			}
		}

		if d, err := strconv.Atoi(digits); err == nil && isGear {
			gears[gearCoord] = append(gears[gearCoord], d)
		}
		digits = ""
		isGear = false
	}

	answer := 0
	for _, v := range gears {
		if len(v) == 2 {
			answer += v[0] * v[1]
		}
	}

	return answer
}

func isGearCheck(x int, y int, board []string) (int, int, bool) {
	for yy := -1; yy < 2; yy++ {
		cY := y + yy

		if cY < 0 || cY >= len(board) {
			continue
		}

		for xx := -1; xx < 2; xx++ {
			cX := x + xx

			if cX < 0 || cX >= len(board[cY]) {
				continue
			}

			c := string(board[cY][cX])

			if c == "*" {
				return cX, cY, true
			}
		}
	}

	return 0, 0, false
}

func main() {
	fmt.Println(day3Part2())
}
