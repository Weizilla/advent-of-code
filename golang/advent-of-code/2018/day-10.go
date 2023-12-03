package main

import (
	"advent-of-code/solution"
	"advent-of-code/utilities"
	"fmt"
	"regexp"
)

type Point struct {
	position *Coord
	velocity Coord
}

type Coord struct {
	x int
	y int
}

type MinMax struct {
	min Coord
	max Coord
}

func (m MinMax) area() int {
	return (m.max.x - m.min.x) * (m.max.y - m.min.y)
}

func day10Part1() any {
	input := solution.ReadInputStrings()
	re := regexp.MustCompile(`<\s*([\d-]+),\s*([\d-]+)>`)

	points := []Point{}
	for _, line := range input {
		found := re.FindAllStringSubmatch(line, -1)
		pos := &Coord{utilities.ToInt(found[0][1]), utilities.ToInt(found[0][2])}
		vel := Coord{utilities.ToInt(found[1][1]), utilities.ToInt(found[1][2])}

		point := Point{pos, vel}
		points = append(points, point)

		fmt.Println(line, point)
	}

	minMax := findMinMax(points)
	area := minMax.area()
	minBoard := ""
	var minPoints = copyBoard(points)

	for i := 0; i < 100000; i++ {
		fmt.Println("Move ", i)
		movePoints(points)
		minMax = findMinMax(points)

		fmt.Printf("area %v %v %v\n", i, area, minMax.area())
		if minMax.area() < area {
			area = minMax.area()
			minPoints = copyBoard(points)
		}
	}

	minBoard = printBoard(minPoints, findMinMax(minPoints))

	return minBoard
}

func day10Part2() any {
	input := solution.ReadInputStrings()
	re := regexp.MustCompile(`<\s*([\d-]+),\s*([\d-]+)>`)

	points := []Point{}
	for _, line := range input {
		found := re.FindAllStringSubmatch(line, -1)
		pos := &Coord{utilities.ToInt(found[0][1]), utilities.ToInt(found[0][2])}
		vel := Coord{utilities.ToInt(found[1][1]), utilities.ToInt(found[1][2])}

		point := Point{pos, vel}
		points = append(points, point)

		fmt.Println(line, point)
	}

	minMax := findMinMax(points)
	area := minMax.area()
	//minBoard := ""
	//var minPoints = copyBoard(points)
	minI := 0

	for i := 0; i < 100000; i++ {
		fmt.Println("Move ", i)
		movePoints(points)
		minMax = findMinMax(points)

		fmt.Printf("area %v %v %v\n", i, area, minMax.area())
		if minMax.area() < area {
			area = minMax.area()
			//minPoints = copyBoard(points)
			minI = i
		}
	}

	//minBoard = printBoard(minPoints, findMinMax(minPoints))

	return minI
}

func findMinMax(points []Point) MinMax {
	minX := 0
	minY := 0
	maxX := 0
	maxY := 0

	for _, point := range points {
		minX = min(point.position.x, minX)
		maxX = max(point.position.x, maxX)
		minY = min(point.position.y, minY)
		maxY = max(point.position.y, maxY)
	}

	return MinMax{min: Coord{minX, minY}, max: Coord{maxX, maxY}}
}

func movePoints(points []Point) {
	for _, point := range points {
		vel := point.velocity
		point.position.x += vel.x
		point.position.y += vel.y
	}
}

func copyBoard(points []Point) []Point {
	newPoints := []Point{}
	for _, p := range points {
		newP := Point{
			velocity: Coord{p.velocity.x, p.velocity.y},
			position: &Coord{p.position.x, p.position.y},
		}

		newPoints = append(newPoints, newP)
	}

	return newPoints
}

func printBoard(points []Point, minMax MinMax) string {
	output := ""

	positions := make(map[Coord]struct{})

	for _, point := range points {
		positions[*point.position] = struct{}{}
	}

	for y := minMax.min.y; y <= minMax.max.y; y++ {
		for x := minMax.min.x; x <= minMax.max.x; x++ {
			if _, ok := positions[Coord{x, y}]; ok {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Printf("\n")
	}

	return output
}

func main() {
	fmt.Println(day10Part2())
}
