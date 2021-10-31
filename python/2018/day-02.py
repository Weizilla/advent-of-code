from collections import Counter
from typing import Union

from solution import Solution


class Day2(Solution):

    def __init__(self) -> None:
        super().__init__(2)

    def part1(self) -> Union[str, int]:
        all_num_twice = 0
        all_num_trice = 0

        for word in self.read_input():
            letter_count = Counter(word)
            has_twice = len([l for (l, n) in letter_count.items() if n == 2]) > 0
            has_trice = len([l for (l, n) in letter_count.items() if n == 3]) > 0

            if has_twice:
                all_num_twice += 1

            if has_trice:
                all_num_trice += 1

        return all_num_twice * all_num_trice

    @staticmethod
    def count_letter_occr(input: str, num_occur: int) -> int:
        counter = Counter(input)
        return len([l for (l, n) in counter.items() if n == num_occur])

    def part2(self) -> Union[str, int]:
        inputs = self.read_input()

        for i in range(len(inputs[0])):
            common_id = self.find_common_ids(inputs, i)
            if common_id:
                return common_id

    def find_common_ids(self, inputs: [str], remove_index: int) -> str:
        ids = set()
        for input in inputs:
            input = input[:remove_index - 1] + input[remove_index:len(input)]
            if input in ids:
                return input
            else:
                ids.add(input)


if __name__ == "__main__":
    Day2().run()
