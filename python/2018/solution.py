import abc
from pathlib import Path
from typing import Optional
from typing import Union

import colorama


class Solution(abc.ABC):
    @abc.abstractmethod
    def __init__(self, day: int) -> None:
        colorama.init()
        self.day = day

    @abc.abstractmethod
    def part1(self) -> Union[str, int]:
        pass

    @abc.abstractmethod
    def part2(self) -> Union[str, int]:
        pass

    def read_input(self, example: Optional[int] = None) -> [str]:
        file_type = f"example-{example}" if example else "input"
        path = (Path(__file__).parent / f"inputs/day-{self.day:02}-{file_type}.txt").resolve()
        with open(path, "r") as f:
            all_lines = f.read().strip()
            return [r.strip() for r in all_lines.splitlines()]

    def read_int_input(self, example: Optional[int]) -> [int]:
        return [int(i) for i in self.read_input(example)]

    def run(self):
        results = self.part2()
        part = 2

        if results is None:
            results = self.part1()
            part = 1

        if results is not None:
            print(f"{colorama.Fore.LIGHTBLACK_EX}Day {self.day} part {part} results:")
            print(f"{colorama.Fore.RED}{results}")
