import { Solution } from "../solution";
import { print } from "../utils";

class Day9 extends Solution {
  constructor(example?: number) {
    super(9, 2021, example);
  }

  part1(): number | string | undefined {
    const grid = this.readInput().map(i => i.split("").map(n => parseInt(n, 10)));
    const maxY = grid.length;
    const maxX = grid[0].length;
    let lowPoints = 0;

    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        const toChecks = this.surround(x, y, maxX, maxY);
        const numHigher = toChecks
          .map(([nX, nY]) => grid[nY][nX])
          .filter(n => n > grid[y][x])
          .length;

        if (numHigher === toChecks.length) {
          lowPoints += grid[y][x] + 1;
        }
      }
    }
    return lowPoints;
  }

  surround(x: number, y: number, maxX: number, maxY: number): number[][] {
    return [
      [x - 1, y],
      [x + 1, y],
      [x, y - 1],
      [x, y + 1],
    ].filter(([nX, nY]) => nX >= 0 && nX < maxX && nY >= 0 && nY < maxY);
  }

  part2(): number | string | undefined {
    const grid = this.readInput().map(i => i.split("").map(n => parseInt(n, 10)));
    const maxY = grid.length;
    const maxX = grid[0].length;
    const seen = new Set<string>();
    const basinSizes = [];

    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        if (grid[y][x] !== 9) {
          if (!seen.has(`${x},${y}`)) {
            const basinSize = this.findBasin(grid, x, y, seen);
            basinSizes.push(basinSize);
            print(`size: ${x},${y} ${basinSize}`);
          }
        }
      }
    }

    const [a, b, c] = [...basinSizes.sort((a, b) => (b - a))];
    print(basinSizes);

    // print(this.findBasin(grid, 5, 0, new Set<string>()));

    return a * b * c;
  }

  findBasin(grid: number[][], x: number, y: number, seen: Set<string>): number {
    const maxY = grid.length;
    const maxX = grid[0].length;
    const prevSeen = seen.size;
    const totalToCheck = [[x, y]];
    seen.add(`${x},${y}`);

    while (totalToCheck.length > 0) {
      const [checkX, checkY] = [...totalToCheck.pop()!];
      print(`check ${checkX}, ${checkY}`);
      const toCheck = this.surround(checkX, checkY, maxX, maxY)
        .filter(([nX, nY]) => grid[nY][nX] !== 9)
        .filter(([nX, nY]) => !seen.has(`${nX},${nY}`));

      print(`to check ${toCheck.join("|")}`);
      toCheck.forEach(([nX, nY]) => seen.add(`${nX},${nY}`));
      toCheck.forEach(([nX, nY]) => totalToCheck.push([nX, nY]));
    }

    return seen.size - prevSeen;
  }
}

(new Day9()).run();
