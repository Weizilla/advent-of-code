package utilities

import "strconv"

func ToInt(input string) int {
	value, err := strconv.Atoi(input)
	if err != nil {
		panic(err)
	}
	return value
}

func Values[M ~map[K]V, K comparable, V any](m M) []V {
	r := make([]V, 0, len(m))
	for _, v := range m {
		r = append(r, v)
	}
	return r
}

func Keys[M ~map[K]V, K comparable, V any](m M) []K {
	r := make([]K, 0, len(m))
	for k := range m {
		r = append(r, k)
	}
	return r
}

func Sum(ints []int) int {
	sum := 0
	for _, v := range ints {
		sum += v
	}
	return sum
}
