import { Solution } from "../solution";
import { HashKey, HashMap, HashSet, Point } from "../collections";
import { print, range } from "../utils";

class Transform extends Point {

}

class Rotation extends Transform {
}

const ROTATIONS = [
  [Rotation.z(0)],
  [Rotation.z(1)],
  [Rotation.z(2)],
  [Rotation.z(3)],

  [Rotation.y(1), Rotation.x(0)],
  [Rotation.y(1), Rotation.x(1)],
  [Rotation.y(1), Rotation.x(2)],
  [Rotation.y(1), Rotation.x(3)],

  [Rotation.y(2), Rotation.z(0)],
  [Rotation.y(2), Rotation.z(1)],
  [Rotation.y(2), Rotation.z(2)],
  [Rotation.y(2), Rotation.z(3)],

  [Rotation.y(0), Rotation.x(3)],
  [Rotation.y(1), Rotation.x(3)],
  [Rotation.y(2), Rotation.x(3)],
  [Rotation.y(3), Rotation.x(3)],

  [Rotation.x(1), Rotation.y(0)],
  [Rotation.x(1), Rotation.y(1)],
  [Rotation.x(1), Rotation.y(2)],
  [Rotation.x(1), Rotation.y(3)],

  [Rotation.x(3), Rotation.y(0)],
  [Rotation.x(3), Rotation.y(1)],
  [Rotation.x(3), Rotation.y(2)],
  [Rotation.x(3), Rotation.y(3)],
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

class RotationId implements HashKey {
  id: number;
  rotation: Rotation;

  constructor(id: number, rotation: Rotation) {
    this.id = id;
    this.rotation = rotation;
  }

  toString(): string {
    return `${this.id}|${this.rotation}`;
  }
}

class Day19 extends Solution {
  rotations: HashMap<RotationId, Point[]>;


  constructor(example?: number) {
    super(19, 2021, example);
    this.rotations = new HashMap<RotationId, Point[]>();
  }

  part1(): number | string {
    const scanners = this.readScanners();

    // const overlaps2 = this.calcOverlapPoints(scanners[3].points, scanners[8]);
    // const overlaps = this.calcOverlapPoints(scanners[8].points, scanners[3]);
    // return overlaps.length;
    return this.runPart1c(scanners);
    // return this.runPart1b(scanners);
  }

  runPart1b(scanners: Scanner[]) {
    const matchedScanners = new Set<number>();

    for (let i = 0; i < scanners.length; i++) {
      for (let j = i + 1; j < scanners.length; j++) {
        const iPoints = scanners[i].points;
        const overlaps = this.calcOverlapPoints(iPoints, scanners[j]);
        if (overlaps.length >= 12) {
          print(`matched ${i} ${j}`);
          matchedScanners.add(i);
          matchedScanners.add(j);
        }
      }
    }

    print(Array.from([...matchedScanners.values()]));
    print(range(scanners.length).filter(r => !matchedScanners.has(r)));

    return matchedScanners.size;
  }

  runPart1c(scanners: Scanner[]): number {
    const merged = new Map<number, Point[]>();
    merged.set(scanners[0].id, scanners[0].points);

    let toMerge = range(scanners.length, 1);

    const compared = new HashSet<string>();

    let numCompared = 0;

    while (toMerge.length > 0) {
      const thisRound = [...toMerge];
      toMerge = [];

      for (const toMergeId of thisRound) {
        const scanner = scanners[toMergeId];

        let didMerge = false;
        for (const mergedId of merged.keys()) {
          numCompared++;

          const comparedKey = `${mergedId}:${scanner.id}`;
          if (!compared.has(comparedKey)) {
            compared.add(comparedKey);
            const mergedPoints = merged.get(mergedId)!;
            const points = this.calcOverlapPoints(mergedPoints, scanner);

            print(`Comparing merged=${mergedId} toMerge=${scanner.id} skipped=f merged=${points.length > 0} numMerged=${merged.size} comparedCache=${compared.size()} numCompared=${numCompared}`);

            if (mergedId === 8 && scanner.id === 3) {
              print(`${mergedId} ${mergedPoints}`);
              print(`${scanner.id} ${scanner.points}`);
            }

            if (points.length > 0) {
              merged.set(toMergeId, points);
              didMerge = true;
              break;
            }
          } else {
            print(`Comparing merged=${mergedId} toMerge=${scanner.id} skipped=t numMerged=${merged.size} comparedCache=${compared.size()} numCompared=${numCompared}`);
          }
        }

        if (!didMerge) {
          toMerge.push(toMergeId);
        }
      }

      if (toMerge.length === thisRound.length) {
        print(`Didn't do any merging ${toMerge}`, 0, "red");
        break;
      }
    }

    if (merged.size === scanners.length) {
      const allPoints = new HashSet<Point>();
      Array.from(merged.values()).forEach(p => allPoints.add(...p));
      return allPoints.size();
    } else {
      print(`Didn't merge all scanners ${merged.size} ${scanners.length}`, 0, "red");
      return 0;
    }
  }

  runPart1(scanners: Scanner[]) {
    const basePoints = new Map<number, Point[]>();
    basePoints.set(scanners[0].id, scanners[0].points);

    let toMerge = range(scanners.length, 1);

    const compared = new Map<number, number[]>();

    while (toMerge.length > 0) {
      const thisRound = [...toMerge];
      toMerge = [];
      for (const i of thisRound) {
        print(`Searching scanner=${i} left=${toMerge.length} base size=${basePoints.size} cached size=${this.rotations.size()}`);
        const scanner = scanners[i];
        const merged = this.merge(basePoints, compared, scanner);
        if (!merged) {
          toMerge.push(i);
        }
      }

      if (toMerge.length === thisRound.length) {
        print(`Nothing got merged: ${toMerge}`);
        return toMerge.length;
      }
    }

    const allPoints = new HashSet<Point>();
    Array.from(basePoints.values()).forEach(p => allPoints.add(...p));
    return allPoints.size();
  }

  private merge(basePoints: Map<number, Point[]>, compared: Map<number, number[]>, scanner: Scanner): boolean {
    for (const baseNum of basePoints.keys()) {
      const hasCompared = compared.get(baseNum) || [];
      if (hasCompared.indexOf(scanner.id) === -1) {
        hasCompared.push(scanner.id);
        compared.set(baseNum, hasCompared);

        const points = this.calcOverlapPoints(basePoints.get(baseNum)!, scanner);
        if (points.length > 0) {
          basePoints.set(scanner.id, points);
          print(`merged ${scanner.id}`);
          return true;
        }
      }
    }

    return false;
  }

  private calcOverlapPoints(basePoints: Point[], scanner: Scanner): Point[] {
    for (const basePoint of basePoints) {
      for (const rotMatrix of ROTATIONS) {
        const rotation = this.rotate(scanner, rotMatrix);
        for (const rotationPoint of rotation) {
          const rotationTranslated = this.translate(basePoint, rotationPoint, rotation);
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

  private rotate(scanner: Scanner, rotations: Rotation[]): Point[] {
    let {points} = scanner;
    for (const rotation of rotations) {
      points = points.map(p => this.rotatePoint(p, rotation));
    }
    return points;
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
    return newPoints;
  }

  private sort(points: Point[]): Point[] {
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
    return points;
  }

  part2(): number | string {
    return 0;
  }
}

(new Day19()).run();
