import { Solution } from "../solution";
import { Grid, HashSet, Point } from "../collections";

class Day15 extends Solution {
  constructor(example?: number) {
    super(15, 2021, example);
  }

  part1(): number | string {
    const grid = new Grid(this.readInput());
    this.print(grid);

    const cost = this.findPathCost(grid);

    return cost;
  }

  private findPathCost(grid: Grid): number {
    let curr: Point = new Point(0, 0);
    let currCost;
    const end = new Point(grid.maxX - 1, grid.maxY - 1);

    const seen = new HashSet<Point>();
    seen.add(curr);
    const toExplore: [[number, Point]] = [[0, curr]];

    while (toExplore.length > 0) {
      toExplore.sort(([n1, _1], [n2, _2]) => (n2 - n1));
      const newVar: [number, Point] = toExplore.pop()!;
      [currCost, curr] = [...newVar];

      if (curr.x === end.x && curr.y === end.y) {
        return currCost;
      }

      const nexts = grid.surround(curr);
      for (const next of nexts) {
        if (!seen.has(next)) {
          const value = grid.value(next);
          toExplore.push([currCost + value, next]);
          seen.add(next);
        }
      }
    }

    return 0;
  }

  part2(): number | string {
    const grid = new Grid(this.readInput());
    this.print(grid);
    this.expandGrid(grid);
    this.print(grid);

    const cost = this.findPathCost(grid);

    return cost;
  }

  expandGrid(grid: Grid) {
    const maxX = grid.maxX;
    const maxY = grid.maxY;

    for (let x = 0; x < maxX; x++) {
      for (let y = 0; y < maxY; y++) {
        const point = new Point(x, y);
        const value = grid.value(point);

        for (let nY = 0; nY < 5; nY++) {
          for (let nX = 0; nX < 5; nX++) {
            if (nX === 0 && nY === 0) {
              continue;
            }

            let newValue = (value + nX + nY) % 9;
            if (newValue === 0) {
              newValue = 9;
            }
            grid.set(new Point(x + nX * maxX, y + nY * maxY), newValue);
          }
        }
      }
    }
  }
}

(new Day15()).run();
