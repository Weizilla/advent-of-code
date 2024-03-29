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
        self.example = None

    def part1(self) -> Union[str, int]:
        raise NotImplementedError("Not implemented")

    def part2(self) -> Union[str, int]:
        raise NotImplementedError("Not implemented")

    def read_input(self) -> [str]:
        file_type = f"example-{self.example}" if self.example else "input"
        path = (Path(__file__).parent / f"inputs/day-{self.day:02}-{file_type}.txt").resolve()
        with open(path, "r") as f:
            all_lines = f.read().strip()
            return [r.strip() for r in all_lines.splitlines()]

    def read_int_input(self) -> [int]:
        return [int(i) for i in self.read_input()]

    def run(self, example: Optional[int] = None):
        part = None
        results = None
        self.example = example

        try:
            results = self.part2()
            part = 2
        except NotImplementedError:
            pass

        if not part:
            try:
                results = self.part1()
                part = 1
            except NotImplementedError:
                pass

        if part:
            print(f"{colorama.Fore.LIGHTBLACK_EX}Day {self.day} part {part} results:")
            print(f"{colorama.Fore.RED}{results}")
        else:
            print(f"{colorama.Fore.LIGHTBLACK_EX}Day {self.day} not done")
