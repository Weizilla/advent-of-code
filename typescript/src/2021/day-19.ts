import { Solution } from "../solution";
import { HashKey, HashMap, HashSet, Point } from "../collections";
import { print, range } from "../utils";

class Transform extends Point {

}

class Rotation extends Transform {
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
  basePoints: Map<number, Point[]>;
  compared: Map<number, number[]>;

  constructor(example?: number) {
    super(19, 2021, example);
    this.rotations = new HashMap<RotationId, Point[]>();
    this.basePoints = new Map<number, Point[]>();
    this.compared = new Map<number, number[]>();
  }

  part1(): number | string | undefined {
    const scanners = this.readScanners();

    this.basePoints.set(0, scanners[0].points);
    let toMerge = range(scanners.length, 1);

    while (toMerge.length > 0) {
      const thisRound = [...toMerge];
      toMerge = [];
      for (const i of thisRound) {
        print(`Searching scanner=${i} left=${toMerge.length} base size=${this.basePoints.size} cached size=${this.rotations.size()}`);
        const scanner = scanners[i];
        const points = this.mergePoints(scanner);
        if (points.length > 0) {
          this.basePoints.set(i, points);
          print(`merged ${i}`);
        } else {
          toMerge.push(i);
        }
      }

      if (toMerge.length === thisRound.length) {
        print("Nothing got merged");
        return 0;
      }
    }

    const allPoints = new HashSet<Point>();
    Array.from(this.basePoints.values()).forEach(p => allPoints.add(...p));
    return allPoints.size();
  }

  private mergePoints(scanner: Scanner): Point[] {
    const start = Date.now();
    let numCompare = 0;
    for (const i of this.basePoints.keys()) {
      const compared = this.compared.get(i) || [];
      if (compared.indexOf(scanner.id) === -1) {
        compared.push(scanner.id);
        this.compared.set(i, compared);

        const basePoints = this.basePoints.get(i)!;
        for (const basePoint of basePoints) {
          for (const rotMatrix of ROTATIONS) {
            const rotation = this.rotate(scanner, rotMatrix);
            for (const rotationPoint of rotation) {
              const rotationTranslated = this.translate(basePoint, rotationPoint, rotation);
              numCompare++;
              const numMatch = this.numMatch(basePoints, rotationTranslated);
              if (numMatch >= 12) {
                print(`Successful merged ${Date.now() - start}`);
                return rotationTranslated;
              }
            }
          }
        }
      }
    }
    print(`No merge took ${Date.now() - start} num compare=${numCompare}`, 0, "red");

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

  private rotate(scanner: Scanner, rotation: Rotation): Point[] {
    const rotationId = new RotationId(scanner.id, rotation);
    const cached = this.rotations.get(rotationId);
    if (cached) {
      return cached;
    }

    const {points} = scanner;
    const newPoints = points.map(p => this.rotatePoint(p, rotation));
    this.rotations.set(rotationId, newPoints);
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
