import re
from collections import defaultdict
from dataclasses import dataclass
from dataclasses import field
from typing import Tuple
from typing import Union

from solution import Solution


@dataclass
class Zone:
    id: str
    center_x: int
    center_y: int
    inf: bool = field(default=False)
    points: [Tuple[int, int]] = field(default_factory=list)


class Day6(Solution):

    def __init__(self) -> None:
        super().__init__(6)

    def part1(self) -> Union[str, int]:
        zones = self.parse_input()

        max_x = max([z.center_x for z in zones]) + 1
        max_y = max([z.center_y for z in zones]) + 1

        grid = self.build_grid_part_1(max_x, max_y, zones)

        # self.print_grid(grid)

        for y in (0, max_y - 1):
            for x in range(max_x):
                point_zones = grid[(x, y)]
                if len(point_zones) == 1:
                    point_zones[0].inf = True

        for y in range(max_y):
            for x in (0, max_x - 1):
                point_zones = grid[(x, y)]
                if len(point_zones) == 1:
                    point_zones[0].inf = True

        max_zone = max([z for z in zones if not z.inf], key=lambda z: len(z.points))
        print(max_zone)

        return len(max_zone.points)

    def build_grid_part_1(self, max_x, max_y, zones):
        grid = defaultdict(list)
        for y in range(max_y):
            for x in range(max_x):
                min_dist = None
                point_dists = defaultdict(list)

                for zone in zones:
                    dist = self.calc_dist(x, y, zone)
                    point_dists[dist].append(zone)

                    if min_dist is None or dist < min_dist:
                        min_dist = dist

                min_dist_zones = point_dists[min_dist]
                if len(min_dist_zones) == 1:
                    for zone in min_dist_zones:
                        zone.points.append((x, y))

                grid[(x, y)] = min_dist_zones

        return grid

    def print_grid(self, grid: dict[Tuple[int, int], [Zone]]) -> None:
        max_x = max(grid.keys(), key=lambda p: p[0])[0] + 1
        max_y = max(grid.keys(), key=lambda p: p[1])[1] + 1

        for y in range(max_y):
            for x in range(max_x):
                zones = grid[(x, y)]
                if len(zones) == 1:
                    zone = zones[0]
                    if (x, y) == (zone.center_x, zone.center_y):
                        id = zone.id.upper()
                    else:
                        id = zone.id
                    print(id, end="")
                else:
                    print(".", end="")

            print("")

    def calc_dist(self, x: int, y: int, zone: Zone) -> int:
        return abs(zone.center_x - x) + abs(zone.center_y - y)

    def parse_input(self) -> [Zone]:
        input = self.read_input()
        zones = []

        for i, line in enumerate(input):
            match = re.match("(\d+), (\d+)", line)
            id = chr(ord("a") + i)
            x = int(match.group(1))
            y = int(match.group(2))
            zones.append(Zone(id, x, y))

        return zones

    def part2(self) -> Union[str, int]:
        zones = self.parse_input()

        max_x = max([z.center_x for z in zones]) + 1
        max_y = max([z.center_y for z in zones]) + 1

        grid = self.build_grid_part_2(max_x, max_y, zones)

        # self.print_grid_part_2(grid)

        return len([v for v in grid.values() if v])

    def build_grid_part_2(self, max_x, max_y, zones):
        grid = {}
        dist_limit = 10000

        for y in range(max_y):
            for x in range(max_x):
                total_dist = 0

                for zone in zones:
                    dist = self.calc_dist(x, y, zone)
                    total_dist += dist

                grid[(x, y)] = total_dist < dist_limit

        return grid

    def print_grid_part_2(self, grid: dict[Tuple[int, int], bool]) -> None:
        max_x = max(grid.keys(), key=lambda p: p[0])[0] + 1
        max_y = max(grid.keys(), key=lambda p: p[1])[1] + 1

        for y in range(max_y):
            for x in range(max_x):
                dist = grid[(x, y)]
                if dist:
                    print("#", end="")
                else:
                    print(".", end="")

            print("")


if __name__ == "__main__":
    Day6().run()
