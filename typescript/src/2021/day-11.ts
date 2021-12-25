import { Solution } from "../solution";
import { print } from "../utils";

class Day11 extends Solution {
  constructor(example?: number) {
    super(11, 2021, example);
  }

  part1(): number | string {
    const grid = this.readInput().map(n => n.split("").map(i => parseInt(i, 10)));
    let flashed = 0;
    const numSteps = 100;
    for (let n = 0; n < numSteps; n++) {
      // this.printGrid(grid);
      flashed += this.runStep(grid);
    }
    // this.printGrid(grid);

    return flashed;
  }

  runStep(grid: number[][]): number {
    const maxX = grid[0].length;
    const maxY = grid.length;
    const flashed = new Set<string>();
    let numFlashed;
    let totalFlashed = 0;

    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        grid[y][x] += 1;
      }
    }

    while (numFlashed === undefined || numFlashed > 0) {
      const before = flashed.size;
      for (let y = 0; y < maxY; y++) {
        for (let x = 0; x < maxX; x++) {
          const gridVal = grid[y][x];
          const flashedKey = `${x},${y}`;

          if (gridVal > 9 && !flashed.has(flashedKey)) {
            this.surround(x, y, maxX, maxY).forEach(([nX, nY]) => {
              grid[nY][nX] += 1;
            });
            flashed.add(flashedKey);
          }
        }
      }
      numFlashed = flashed.size - before;
    }

    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        const gridVal = grid[y][x];
        if (gridVal > 9) {
          grid[y][x] = 0;
          totalFlashed += 1;
        }
      }
    }

    return totalFlashed;
  }

  printGrid(grid: number[][]) {
    const maxX = grid[0].length;
    const maxY = grid.length;
    let output = "";
    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        output += ` ${grid[y][x].toString().padStart(2, " ")}`;
      }
      output += "\n";
    }
    print(output);
  }

  surround(x: number, y: number, maxX: number, maxY: number): number[][] {
    return [
      [x - 1, y],
      [x + 1, y],
      [x, y - 1],
      [x, y + 1],
      [x - 1, y - 1],
      [x + 1, y + 1],
      [x - 1, y + 1],
      [x + 1, y - 1],
    ].filter(([nX, nY]) => nX >= 0 && nX < maxX && nY >= 0 && nY < maxY);
  }

  part2(): number | string {
    const grid = this.readInput().map(n => n.split("").map(i => parseInt(i, 10)));
    let numSteps = 0;
    let all = this.allFlashed(grid);
    while (!all) {
      this.runStep(grid);
      numSteps++;
      // print(numSteps);
      all = this.allFlashed(grid);
    }
    return numSteps;
  }

  allFlashed(grid: number[][]): boolean {
    const maxX = grid[0].length;
    const maxY = grid.length;
    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        if (grid[y][x] > 0) {
          return false;
        }
      }
    }
    return true;
  }
}

(new Day11()).run();
