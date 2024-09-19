package main

import (
	"advent-of-code/solution"
	"advent-of-code/utilities"
	"fmt"
	"strings"
)

type Type string

const (
	SEED  Type = "seed"
	SOIL  Type = "soil"
	FERT  Type = "fert"
	WATER Type = "water"
	LIGHT Type = "light"
	TEMP  Type = "temp"
	HUM   Type = "hum"
	LOC   Type = "loc"
)

var allTypes = []Type{
	SEED,
	SOIL,
	FERT,
	WATER,
	LIGHT,
	TEMP,
	HUM,
}

var maplookup = map[string]Type{
	"seed-to-soil map:":            SEED,
	"soil-to-fertilizer map:":      SOIL,
	"fertilizer-to-water map:":     FERT,
	"water-to-light map:":          WATER,
	"light-to-temperature map:":    LIGHT,
	"temperature-to-humidity map:": TEMP,
	"humidity-to-location map:":    HUM,
}

type Mapping struct {
	sourceStart int
	destStart   int
	rangeLen    int
}

func (m Mapping) MapValue(value int) (int, bool) {
	if value < m.sourceStart || value >= m.sourceStart+m.rangeLen {
		return 0, false
	}

	return m.destStart + (value - m.sourceStart), true
}

func day5Part1() any {
	input := solution.ReadInputStrings()

	allMaps := map[Type][]Mapping{}
	typeToFill := SEED
	mapToFill := []Mapping{}
	seeds := []int{}

	for _, line := range input {
		if line[:len("seeds")] == "seeds" {
			l := strings.Split(line, ":")
			for _, s := range strings.Split(strings.TrimSpace(l[1]), " ") {
				seeds = append(seeds, utilities.ToInt(s))
			}
		} else if strings.Index(line, "map") != -1 {
			typeToFill = maplookup[line]
			mapToFill = []Mapping{}
			allMaps[typeToFill] = mapToFill
		} else if len(strings.TrimSpace(line)) > 0 {
			m := strings.Split(line, " ")
			ma := Mapping{
				destStart:   utilities.ToInt(m[0]),
				sourceStart: utilities.ToInt(m[1]),
				rangeLen:    utilities.ToInt(m[2]),
			}
			mapToFill = append(mapToFill, ma)
			allMaps[typeToFill] = mapToFill
		}
	}

	locations := map[int]int{}

	for _, seed := range seeds {
		curr := seed
		for _, t := range allTypes {
			maps := allMaps[t]
			for _, m := range maps {
				newCurr, ok := m.MapValue(curr)
				if ok {
					curr = newCurr
					break
				}
			}
		}

		locations[curr] = seed
	}

	minValue := -1
	for k := range locations {
		if minValue == -1 || k < minValue {
			minValue = k
		}
	}

	return minValue
}

type Saved struct {
	t Type
	v int
}

func day5Part2() any {
	input := solution.ReadInputStrings()

	allMaps := map[Type][]Mapping{}
	typeToFill := SEED
	mapToFill := []Mapping{}
	inputSeeds := []int{}

	for _, line := range input {
		if line[:len("seeds")] == "seeds" {
			l := strings.Split(line, ":")
			for _, s := range strings.Split(strings.TrimSpace(l[1]), " ") {
				inputSeeds = append(inputSeeds, utilities.ToInt(s))
			}
		} else if strings.Index(line, "map") != -1 {
			typeToFill = maplookup[line]
			mapToFill = []Mapping{}
			allMaps[typeToFill] = mapToFill
		} else if len(strings.TrimSpace(line)) > 0 {
			m := strings.Split(line, " ")
			ma := Mapping{
				destStart:   utilities.ToInt(m[0]),
				sourceStart: utilities.ToInt(m[1]),
				rangeLen:    utilities.ToInt(m[2]),
			}
			mapToFill = append(mapToFill, ma)
			allMaps[typeToFill] = mapToFill
		}
	}

	locations := map[int]int{}

	saved := map[Saved]int{}

	steps := []Saved{}

	for s := 0; s < len(inputSeeds); s += 2 {
		seedStart := inputSeeds[s]
		seedRange := inputSeeds[s+1]
		for seed := seedStart; seed < seedStart+seedRange; seed++ {
			curr := seed
			for _, t := range allTypes {
				if cached, ok := saved[Saved{t, curr}]; ok {
					curr = cached
					fmt.Println("cached")
					break
				}
				steps = append(steps, Saved{t, curr})

				maps := allMaps[t]
				for _, m := range maps {
					newCurr, ok := m.MapValue(curr)
					if ok {
						curr = newCurr
						break
					}
				}
			}

			locations[curr] = seed
			for _, cc := range steps {
				saved[cc] = curr
			}
		}
	}

	minValue := -1
	for k := range locations {
		if minValue == -1 || k < minValue {
			minValue = k
		}
	}

	return minValue

}

func main() {
	fmt.Println(day5Part2())
}
