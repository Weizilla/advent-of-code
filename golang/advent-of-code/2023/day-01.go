package main

import (
	"advent-of-code/solution"
	"fmt"
	"strconv"
)

var words = map[string]int{
	"0":     0,
	"1":     1,
	"2":     2,
	"3":     3,
	"4":     4,
	"5":     5,
	"6":     6,
	"7":     7,
	"8":     8,
	"9":     9,
	"zero":  0,
	"one":   1,
	"two":   2,
	"three": 3,
	"four":  4,
	"five":  5,
	"six":   6,
	"seven": 7,
	"eight": 8,
	"nine":  9,
}

func day1Part1() any {
	inputs := solution.ReadInputStrings()

	totalSum := 0

	for _, line := range inputs {
		lineNums := []int{}
		for _, char := range line {
			num, err := strconv.Atoi(string(char))
			if err == nil {
				fmt.Printf("%v\n", num)
				lineNums = append(lineNums, num)
			}
		}
		first := lineNums[0]
		last := lineNums[len(lineNums)-1]
		lineNum, err := strconv.Atoi(strconv.Itoa(first) + strconv.Itoa(last))
		if err == nil {
			fmt.Printf("line num %v\n", lineNum)
			totalSum += lineNum
		}
	}
	return totalSum
}

func day1Part2() any {
	inputs := solution.ReadInputStrings()
	totalSum := 0

	for _, line := range inputs {

		lineNums := []int{}
		for i := 0; i < len(line); i++ {
			for word, digit := range words {
				if len(line)-i < len(word) {
					continue
				}

				if line[i:i+len(word)] == word {
					lineNums = append(lineNums, digit)
				}
			}
		}

		first := lineNums[0]
		last := lineNums[len(lineNums)-1]
		lineNum, err := strconv.Atoi(strconv.Itoa(first) + strconv.Itoa(last))
		if err == nil {
			fmt.Printf("line num %v\n", lineNum)
			totalSum += lineNum
		}
	}

	return totalSum
}

func main() {
	fmt.Println(day1Part2())
}
