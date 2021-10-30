from utils import read_int_input


def day01_part1() -> int:
    return sum(read_int_input(1))


def day01_part2() -> int:
    input = read_int_input(1)
    seen = set()
    freq = 0
    i = 0
    while True:
        curr = input[i % len(input)]
        freq += curr
        if freq in seen:
            return freq
        else:
            seen.add(freq)

        i += 1


if __name__ == "__main__":
    print(day01_part2())
