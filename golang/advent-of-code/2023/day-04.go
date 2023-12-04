package main

import (
	"advent-of-code/solution"
	"fmt"
	"math"
	"strconv"
	"strings"
)

func day4Part1() any {
	inputs := solution.ReadInputStrings()

	points := 0

	for _, line := range inputs {
		numPoints := 0

		l1 := strings.Split(line, "|")
		l2 := strings.Split(l1[0], ":")

		//cardIdInput := strings.TrimSpace(l2[0])
		//cardIdSplit := strings.Split(cardIdInput, " ")
		//cardId := utilities.ToInt(cardIdSplit[len(cardIdSplit)-1])

		winningNums := map[int]struct{}{}
		w1 := strings.Split(strings.TrimSpace(l2[1]), " ")
		for _, w11 := range w1 {
			if i, err := strconv.Atoi(w11); err == nil {
				winningNums[i] = struct{}{}
			}
		}

		fmt.Println(winningNums)

		inputNumbers := []int{}
		i1 := strings.Split(strings.TrimSpace(l1[1]), " ")
		for _, i11 := range i1 {
			if i, err := strconv.Atoi(i11); err == nil {
				inputNumbers = append(inputNumbers, i)
			}
		}

		fmt.Println(inputNumbers)

		for _, i := range inputNumbers {
			if _, ok := winningNums[i]; ok {
				numPoints++
			}
		}

		newPoints := math.Pow(2, float64(numPoints-1))

		fmt.Println("num points", numPoints, newPoints)

		if numPoints > 0 {
			points += int(newPoints)
		}
	}

	return points
}

func day4Part2() any {
	inputs := solution.ReadInputStrings()

	cardsWon := []int{}

	for _, line := range inputs {
		numWon := 0

		l1 := strings.Split(line, "|")
		l2 := strings.Split(l1[0], ":")

		//cardIdInput := strings.TrimSpace(l2[0])
		//cardIdSplit := strings.Split(cardIdInput, " ")
		//cardId := utilities.ToInt(cardIdSplit[len(cardIdSplit)-1])
		//
		winningNums := map[int]struct{}{}
		w1 := strings.Split(strings.TrimSpace(l2[1]), " ")
		for _, w11 := range w1 {
			if i, err := strconv.Atoi(w11); err == nil {
				winningNums[i] = struct{}{}
			}
		}

		//fmt.Println(winningNums)

		inputNumbers := []int{}
		i1 := strings.Split(strings.TrimSpace(l1[1]), " ")
		for _, i11 := range i1 {
			if i, err := strconv.Atoi(i11); err == nil {
				inputNumbers = append(inputNumbers, i)
			}
		}

		//fmt.Println(inputNumbers)

		for _, i := range inputNumbers {
			if _, ok := winningNums[i]; ok {
				numWon++
			}
		}

		cardsWon = append(cardsWon, numWon)
	}

	return allNumWon(cardsWon)
}

func allNumWon(cards []int) int {
	total := 0
	for i := 0; i < len(cards); i++ {
		total += allNumWonCard(i, cards)
	}

	return total
}

func allNumWonCard(cardId int, cards []int) int {
	total := 1
	numWon := cards[cardId]
	for i := 0; i < numWon; i++ {
		total += allNumWonCard(cardId+i+1, cards)
	}

	return total
}

func main() {
	fmt.Println(day4Part2())
}
