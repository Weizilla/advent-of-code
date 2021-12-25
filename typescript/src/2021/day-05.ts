import { Solution } from "../solution";
import { HashMap } from "../collections";
import { print, range } from "../utils";

class Point {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  toString() {
    return `(${this.x},${this.y})`;
  }
}

class Line {
  start: Point;
  end: Point;

  constructor(start: Point, end: Point) {
    this.start = start;
    this.end = end;
  }

  toString() {
    return `${this.start} => ${this.end}`;
  }

  isHorizontal() {
    return this.start.y === this.end.y;
  }

  isVertical() {
    return this.start.x === this.end.x;
  }
}

class Day5 extends Solution {
  constructor(example?: number) {
    super(5, 2021, example);
  }

  part1(): number | string {
    const lines = this.readInput().map(this.parseLine);
    const hvLines = lines.filter(l => l.isHorizontal() || l.isVertical());

    const overlaps = new HashMap<Point, number>();

    hvLines.forEach(line => {
      if (line.isHorizontal()) {
        const minX = Math.min(line.start.x, line.end.x);
        const maxX = Math.max(line.start.x, line.end.x);
        for (let i = minX; i <= maxX; i++) {
          const point = new Point(i, line.start.y);
          const numOverlaps = overlaps.get(point) || 0;
          overlaps.set(point, numOverlaps + 1);
        }
      } else if (line.isVertical()) {
        const minY = Math.min(line.start.y, line.end.y);
        const maxY = Math.max(line.start.y, line.end.y);
        for (let i = minY; i <= maxY; i++) {
          const point = new Point(line.start.x, i);
          const numOverlaps = overlaps.get(point) || 0;
          overlaps.set(point, numOverlaps + 1);
        }
      }
      // print(line);
      // print(overlaps);
    });

    return overlaps.values().filter(v => v > 1).length;
  }

  parseLine(line: string): Line {
    const splits = line.split(" ");
    const [x, y] = splits[0].split(",").map(n => parseInt(n, 10));
    const [x2, y2] = splits[2].split(",").map(n => parseInt(n, 10));
    return new Line(new Point(x, y), new Point(x2, y2));
  }

  part2(): number | string {
    const lines = this.readInput().map(this.parseLine);

    const overlaps = new HashMap<Point, number>();

    lines.forEach(line => {
      if (line.isHorizontal()) {
        for (const x of range(line.end.x, line.start.x, true)) {
          const point = new Point(x, line.start.y);
          const numOverlaps = overlaps.get(point) || 0;
          overlaps.set(point, numOverlaps + 1);
        }
      } else if (line.isVertical()) {
        for (const y of range(line.end.y, line.start.y, true)) {
          const point = new Point(line.start.x, y);
          const numOverlaps = overlaps.get(point) || 0;
          overlaps.set(point, numOverlaps + 1);
        }
      } else {
        const xs = range(line.end.x, line.start.x, true);
        const ys = range(line.end.y, line.start.y, true);
        for (let i = 0; i < xs.length; i++) {
          const point = new Point(xs[i], ys[i]);
          const numOverlaps = overlaps.get(point) || 0;
          overlaps.set(point, numOverlaps + 1);
        }
      }
      // print(line);
      // print(overlaps);
    });

    // this.printGrid(overlaps);

    return overlaps.values().filter(v => v > 1).length;
  }

  printGrid(overlaps: HashMap<Point, number>) {
    let g = "";
    const maxX = Math.max(...overlaps.keys().map(k => k.x));
    const maxY = Math.max(...overlaps.keys().map(k => k.y));

    for (let y = 0; y <= maxY; y++) {
      for (let x = 0; x <= maxX; x++) {
        const p = new Point(x, y);
        const o = overlaps.get(p) || 0;
        if (o === 0) {
          g += ".";
        } else {
          g += o.toString();
        }
      }
      g += "\n";
    }

    print(g);
  }
}

(new Day5()).run();
