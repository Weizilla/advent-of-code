package solution

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

const NoExample int = -1
const AllExamples int = 0

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func ReadInput(year int, day int, example ...int) string {
	var path string
	if len(example) > 0 {
		exampleId := example[0]
		if exampleId == AllExamples {
			path = fmt.Sprintf("../../inputs/%d/day-%02d-examples.txt", year, day)
		} else if exampleId != NoExample {
			path = fmt.Sprintf("../../inputs/%d/day-%02d-example-%d.txt", year, day, exampleId)
		}
	} else {
		path = fmt.Sprintf("../../inputs/%d/day-%02d-input.txt", year, day)
	}

	dat, err := os.ReadFile(path)
	check(err)

	contents := string(dat)

	return contents
}

func ReadInputStrings(year int, day int, example ...int) []string {
	content := ReadInput(year, day, example...)
	allStrings := strings.Split(content, "\n")

	goodStrings := make([]string, 0)
	for _, n := range allStrings {
		if len(n) > 0 {
			goodStrings = append(goodStrings, n)
		}
	}
	return goodStrings
}

func ReadInputInts(year int, day int, example ...int) []int {
	lines := ReadInputStrings(year, day, example...)

	ints := make([]int, len(lines))
	for i, n := range lines {
		newInt, err := strconv.Atoi(n)
		check(err)
		ints[i] = newInt
	}

	return ints
}
