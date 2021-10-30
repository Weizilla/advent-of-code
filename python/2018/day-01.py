from typing import Union

from solution import Solution


class Day1(Solution):

    def __init__(self) -> None:
        super().__init__(1)

    def part1(self) -> Union[str, int]:
        return sum(self.read_int_input())

    def part2(self) -> Union[str, int]:
        input = self.read_int_input()
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
    Day1().run()
