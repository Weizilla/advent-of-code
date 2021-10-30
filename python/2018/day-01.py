from utils import read_input


def day01_part1() -> int:
    input = read_input(1)
    freqs = [int(a) for a in input]
    curr = 0
    for f in freqs:
        curr += f
    return curr


if __name__ == "__main__":
    print(day01_part1())
