package solution

import (
	"fmt"
	"os"
	"regexp"
	"runtime"
	"strconv"
	"strings"
)

const NoExample int = -1
const AllExamples int = 0

var FileNamePattern, _ = regexp.Compile("([0-9]+)/day-([0-9]+)")

func Check(e error) {
	if e != nil {
		panic(e)
	}
}

func findYearDay() (int, int) {
	for i := 0; i < 10; i++ {
		_, file, _, _ := runtime.Caller(i)
		matches := FileNamePattern.FindStringSubmatch(file)
		if len(matches) > 0 {
			year, _ := strconv.Atoi(matches[1])
			day, _ := strconv.Atoi(matches[2])
			return year, day
		}
	}

	panic("year day not found")
}

func ReadInput(example ...int) string {
	var year, day = findYearDay()
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
	Check(err)

	contents := string(dat)

	return contents
}

func ReadInputStrings(example ...int) []string {
	content := ReadInput(example...)
	allStrings := strings.Split(content, "\n")

	goodStrings := make([]string, 0)
	for _, n := range allStrings {
		if len(n) > 0 {
			goodStrings = append(goodStrings, n)
		}
	}
	return goodStrings
}

func ReadInputInts(example ...int) []int {
	lines := ReadInputStrings(example...)

	ints := make([]int, len(lines))
	for i, n := range lines {
		newInt, err := strconv.Atoi(n)
		Check(err)
		ints[i] = newInt
	}

	return ints
}
