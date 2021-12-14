import { Solution } from "../solution";
import { HashKey, HashSet } from "../collections";
import { print } from "../utils";

class Point implements HashKey {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  toString(): string {
    return `(${this.x},${this.y})`;
  }
}

class Day13 extends Solution {
  constructor(example?: number) {
    super(13, 2021, example);
  }

  part1(): number | string | undefined {
    const lines = this.readInput();
    const points = this.readPoints(lines);

    this.printPoints(points);
    const folds = this.readFolds(lines);

    this.print(points);
    this.print(folds);
    this.fold(folds[0], points);
    this.printPoints(points);
    this.print(points);
    this.print(folds);

    return points.size();
  }

  private fold(fold: string[], points: HashSet<Point>) {
    const foldAxis = fold[0];
    const foldValue = parseInt(fold[1], 10);
    if (foldAxis === "x") {
      const toFold = points.values().filter(p => p.x > foldValue);
      toFold.forEach(p => {
        points.delete(p);
        points.add(new Point(foldValue - Math.abs(p.x - foldValue), p.y));
      });
    } else {
      const toFold = points.values().filter(p => p.y > foldValue);
      toFold.forEach(p => {
        points.delete(p);
        points.add(new Point(p.x, foldValue - Math.abs(p.y - foldValue)));
      });
    }
  }

  private readFolds(lines: string[]) {
    return lines.filter(l => l.indexOf("fold") !== -1)
      .map(l => l.split(" ")[2].split("="));
  }

  private readPoints(lines: string[]) {
    const points = lines.filter(l => l.indexOf("fold") === -1)
      .map(l => l.split(",").map(n => parseInt(n, 10)))
      .map(([x, y]) => new Point(x, y));

    const allPoints = new HashSet<Point>();
    points.forEach(p => allPoints.add(p));
    return allPoints;
  }

  printPoints(points: HashSet<Point>, always: boolean = false) {
    const maxX = Math.max(...points.values().map(p => p.x));
    const maxY = Math.max(...points.values().map(p => p.y));

    let output = "";
    for (let y = 0; y <= maxY; y++) {
      for (let x = 0; x <= maxX; x++) {
        if (points.has(new Point(x, y))) {
          output += "#";
        } else {
          output += " ";
        }
      }
      output += "\n";
    }

    if (always) {
      print(output);
    } else {
      this.print(output);
    }
  }

  part2(): number | string | undefined {
    const lines = this.readInput();
    const points = this.readPoints(lines);

    this.printPoints(points);
    const folds = this.readFolds(lines);

    this.print(points);
    this.print(folds);
    folds.forEach(f => this.fold(f, points));
    this.printPoints(points, true);

    return points.size();
  }
}

(new Day13()).run();
