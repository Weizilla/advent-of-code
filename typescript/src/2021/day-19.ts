import { Solution } from "../solution";
import { HashSet, Point } from "../collections";
import { print, range } from "../utils";

class Transform extends Point {

}

class Rotation extends Transform {
}

class Translation extends Transform {

}

const ROTATIONS = [
  new Rotation(0, 0, 0),
  new Rotation(0, 0, 1),
  new Rotation(0, 0, 2),
  new Rotation(0, 0, 3),

  new Rotation(0, 1, 0),
  new Rotation(1, 1, 0),
  new Rotation(2, 1, 0),
  new Rotation(3, 1, 0),

  new Rotation(0, 2, 0),
  new Rotation(0, 2, 1),
  new Rotation(0, 2, 2),
  new Rotation(0, 2, 3),

  new Rotation(0, 3, 0),
  new Rotation(1, 3, 0),
  new Rotation(2, 3, 0),
  new Rotation(3, 3, 0),

  new Rotation(1, 0, 0),
  new Rotation(1, 1, 0),
  new Rotation(1, 2, 0),
  new Rotation(1, 3, 0),

  new Rotation(3, 0, 0),
  new Rotation(3, 1, 0),
  new Rotation(3, 2, 0),
  new Rotation(3, 3, 0),
];

class Scanner {
  id: number;
  points: Point[];

  constructor(id: number) {
    this.id = id;
    this.points = [];
  }

  toString(): string {
    return `[id=${this.id} ${this.points}]`;
  }
}

class Day19 extends Solution {
  constructor(example?: number) {
    super(19, 2021, example);
  }

  part1(): number | string | undefined {
    const scanners = this.readScanners();

    // this.print(scanners.join("\n"));
    //

    // this.print("0");
    // this.print(scanners[0].points.join("\n"));
    //
    // for (const p of this.allRotations(scanners[0].points)) {
    //   this.print("---------");
    //   this.print(p.join("\n"));
    // }

    const basePoints = new HashSet<Point>(scanners[0].points);
    const toMerge = range(scanners.length);

    while (toMerge.length > 0) {
      const i = toMerge.reverse().pop()!;
      toMerge.reverse();
      const scanner = scanners[i];
      const points = this.mergePoints(basePoints, scanner);
      if (points.length > 0) {
        const len = basePoints.size();
        basePoints.add(...points);
        print(`${i}  before ${len} found ${points.length} after ${basePoints.size()}`);
      } else {
        toMerge.push(i);
      }
    }

    return basePoints.size();
  }

  private mergePoints(basePoints: Iterable<Point>, scanner: Scanner): Point[] {
    print(`Searching scanner ${scanner.id}`);
    for (const basePoint of basePoints) {
      for (const rotMatrix of ROTATIONS) {
        const rotation = this.rotate(scanner.points, rotMatrix);
        for (const r of rotation) {
          const rotationTranslated = this.translate(basePoint, r, rotation);
          const numMatch = this.numMatch(basePoints, rotationTranslated);
          if (numMatch >= 12) {
            return rotationTranslated;
          }
        }
      }
    }

    return [];
  }

  private numMatch(pointsA: Iterable<Point>, pointsB: Iterable<Point>): number {
    const a = new HashSet(pointsA);
    const b = new HashSet(pointsB);
    return a.intersection(b).size;
  }

  private readScanners(): Scanner[] {
    const scanners: Scanner[] = [];
    let curr: Scanner | null = null;
    this.readInput().forEach(line => {
      if (line.indexOf("scanner") > -1) {
        const id = parseInt(line.split(" ")[2], 10);
        curr = new Scanner(id);
        scanners.push(curr);
      } else if (line.indexOf(",") > -1) {
        const splits = line.split(",");
        const [x, y, z] = [...splits.map(s => parseInt(s, 10))];
        const point = new Point(x, y, z);
        curr?.points.push(point);
      }
    });

    return scanners;
  }


  private rotate(points: Point[], rotation: Rotation): Point[] {
    const newPoints = points.map(p => this.rotatePoint(p, rotation));
    this.sort(newPoints);
    return newPoints;
  }

  private rotatePoint(point: Point, rotation: Rotation): Point {
    const rRad = rotation.scale(Math.PI / 2);

    const rx: [Rotation, Rotation, Rotation] = [
      new Rotation(1, 0, 0),
      new Rotation(0, Math.cos(rRad.x), -Math.sin(rRad.x)),
      new Rotation(0, Math.sin(rRad.x), Math.cos(rRad.x)),
    ];
    const ry: [Rotation, Rotation, Rotation] = [
      new Rotation(Math.cos(rRad.y), 0, Math.sin(rRad.y)),
      new Rotation(0, 1, 0),
      new Rotation(-Math.sin(rRad.y), 0, Math.cos(rRad.y)),
    ];
    const rz: [Rotation, Rotation, Rotation] = [
      new Rotation(Math.cos(rRad.z), -Math.sin(rRad.z), 0),
      new Rotation(Math.sin(rRad.z), Math.cos(rRad.z), 0),
      new Rotation(0, 0, 1),
    ];

    let np = point;
    if (rotation.x) {
      np = np.matrixProduct(rx);
    }
    if (rotation.y) {
      np = np.matrixProduct(ry);
    }
    if (rotation.z) {
      np = np.matrixProduct(rz);
    }
    return np;
  }

  private translate(base: Point, reference: Point, points: Point[]): Point[] {
    const t = base.sum(reference.scale(-1));
    const newPoints = points.map(p => p.sum(t));
    this.sort(newPoints);
    return newPoints;
  }

  private sort(points: Point[]) {
    points.sort((p1, p2) => {
      let result = p1.x - p2.x;
      if (result === 0) {
        result = p1.y - p2.y;
      }
      if (result === 0) {
        result = p1.z - p2.z;
      }
      return result;
    });
  }

  part2(): number | string | undefined {
    return undefined;
  }
}

(new Day19()).run();
