import { Solution } from "../solution";
import { Point, StringGrid } from "../collections";

class Day25 extends Solution {
  constructor(example?: number, forcePrint: boolean = false) {
    super(25, 2021, example, forcePrint);
  }

  part1(): number | string {
    const inputs = this.readInput();
    let grid = new StringGrid(inputs);
    this.print(grid);

    let moved = true;
    let i = 0;
    while (moved) {
      [grid, moved] = this.step(grid);
      i++;
      this.print(i);
    }

    return i;
  }

  private step(grid: StringGrid): [StringGrid, boolean] {
    let moved = false;
    let [newGrid, newMoved] = this.move(grid, true);
    moved = moved || newMoved;
    this.print(newGrid);

    [newGrid, newMoved] = this.move(newGrid, false);
    moved = moved || newMoved;
    this.print(newGrid);

    return [newGrid, moved];
  }

  private move(grid: StringGrid, toRight: boolean): [StringGrid, boolean] {
    const newGrid = new StringGrid();
    let moved = false;
    if (toRight) {
      for (let y = grid.minY; y < grid.maxY; y++) {
        for (let x = grid.minX; x < grid.maxX; x++) {
          const point = new Point(x, y);
          const value = grid.value(point);
          if (value === ">") {
            const right = point.x + 1 < grid.maxX ? point.x + 1 : grid.minX;
            const rightPoint = new Point(right, point.y);
            if (grid.value(rightPoint) === ".") {
              newGrid.set(rightPoint, ">");
              newGrid.set(point, ".");
              x++;
              moved = true;
            } else if (value) {
              newGrid.set(point, value);
            }
          } else if (value) {
            newGrid.set(point, value);
          }
        }
      }
    } else {
      for (let x = grid.minX; x < grid.maxX; x++) {
        for (let y = grid.minY; y < grid.maxY; y++) {
          const point = new Point(x, y);
          const value = grid.value(point);
          if (value === "v") {
            const bottom = point.y + 1 < grid.maxY ? point.y + 1 : grid.minY;
            const bottomPoint = new Point(point.x, bottom);
            if (grid.value(bottomPoint) === ".") {
              newGrid.set(bottomPoint, "v");
              newGrid.set(point, ".");
              y++;
              moved = true;
            } else if (value) {
              newGrid.set(point, value);
            }
          } else if (value) {
            newGrid.set(point, value);
          }
        }
      }
    }

    return [newGrid, moved];
  }

  part2(): number | string {
    return 0;
  }
}

(new Day25()).run();
