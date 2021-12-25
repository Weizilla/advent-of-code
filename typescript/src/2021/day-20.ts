import { Solution } from "../solution";
import { Point, StringGrid } from "../collections";
import { binToDec } from "../utils";

class Day20 extends Solution {
  constructor(example?: number) {
    super(20, 2021, example);
  }

  part1(): number | string {
    const inputs = this.readInput();
    const algo = inputs[0].split("");
    let grid = new StringGrid(inputs.slice(1));

    for (let i = 0; i < 2; i++) {
      grid = this.enhance(grid, algo, i % 2 === 0 ? "." : "#");
    }

    return grid.values().filter(v => v === "#").length;
  }

  private enhance(grid: StringGrid, algo: string[], defaultValue: string): StringGrid {
    const newGrid = new StringGrid();
    const padding = 3;
    for (let y = grid.minY - padding; y < grid.maxY + padding; y++) {
      for (let x = grid.minX - padding; x < grid.maxX + padding; x++) {
        const point = new Point(x, y);
        const surround = grid.surroundValues(point, true, defaultValue, true, true);
        const surroundStr = surround.join("").replace(/\./g, "0").replace(/#/g, "1");
        const algoIndex = binToDec(surroundStr);
        const newVal = algo[algoIndex];
        newGrid.set(point, `${newVal}`);
      }
    }
    return newGrid;
  }

  part2(): number | string {
    const inputs = this.readInput();
    const algo = inputs[0].split("");
    let grid = new StringGrid(inputs.slice(1));

    for (let i = 0; i < 50; i++) {
      grid = this.enhance(grid, algo, i % 2 === 0 ? "." : "#");
    }

    return grid.values().filter(v => v === "#").length;
  }
}

(new Day20()).run();
