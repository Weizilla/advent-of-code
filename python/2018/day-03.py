import re
from collections import defaultdict
from dataclasses import dataclass
from typing import Tuple
from typing import Union

from solution import Solution


#  -> x
# |
# y

@dataclass
class Patch:
    id: int
    x: int
    y: int
    width: int
    height: int
    overlap: bool = False


class Day3(Solution):

    def __init__(self) -> None:
        super().__init__(3)

    def part1(self) -> Union[str, int]:
        input = self.read_input()
        patches = [self.parse_line(l) for l in input]

        grid = defaultdict(list)
        for patch in patches:
            for w in range(patch.width):
                for h in range(patch.height):
                    x = patch.x + w
                    y = patch.y + h
                    grid[(x, y)].append(patch.id)

        return len([g for g in grid.values() if len(g) > 1])

    def print_grid(self, patches: [Patch], grid: dict[[Tuple[int, int]], [int]]) -> None:
        max_x = max([p.x + p.width for p in patches]) + 1
        max_y = max([p.y + p.height for p in patches]) + 1
        for y in range(max_y):
            for x in range(max_x):
                ids = grid[(x, y)]
                if len(ids) == 1:
                    print(ids[0], end="")
                elif len(ids) > 1:
                    print("X", end="")
                else:
                    print(".", end="")
            print("")

    def parse_line(self, line: str) -> Patch:
        match = re.match("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", line)
        if match:
            return Patch(*[int(i) for i in match.groups()])
        else:
            raise Exception(f"Invalid line {line}")

    def part2(self) -> Union[str, int]:
        input = self.read_input()
        patches = {p.id: p for p in [self.parse_line(l) for l in input]}

        grid = defaultdict(list)
        for patch in patches.values():
            for w in range(patch.width):
                for h in range(patch.height):
                    x = patch.x + w
                    y = patch.y + h
                    grid_ids = grid[(x, y)]
                    grid_ids.append(patch.id)

                    if len(grid_ids) > 1:
                        for p_id in grid_ids:
                            patches[p_id].overlap = True

        return [p.id for p in patches.values() if not p.overlap][0]


if __name__ == "__main__":
    Day3().run()
