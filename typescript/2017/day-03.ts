import Solution from "./solution";

interface HashKey {
  toString(): string;
}

class HashMap<K extends HashKey, V> {
  map: Map<String, V>;

  constructor() {
    this.map = new Map();
  }

  get(key: K): V | undefined {
    return this.map.get(key.toString());
  }

  set(key: K, value: V) {
    this.map.set(key.toString(), value);
  }

  toString(): string {
    return this.map.toString();
  }
}

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

class Day3 extends Solution {
  constructor(example?: number) {
    super(3, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInputInts();
    const solutions = inputs.map(n => this.solvePart1(n));
    this.print(solutions);
    return solutions[solutions.length - 1];
  }

  solvePart1(memLoc: number): number {
    const halfRoot = Math.floor(Math.ceil(Math.sqrt(memLoc)) / 2);
    const root = halfRoot * 2 + 1;
    const nextCorner = root * root;
    const halfRootDist = Math.floor(root / 2);
    const edgeCenters = [
      nextCorner - halfRootDist,
      nextCorner - (root - 1) - halfRootDist,
      nextCorner - (root - 1) * 2 - halfRootDist,
      nextCorner - (root - 1) * 3 - halfRootDist,
    ];
    const diffToEdge = Math.min(...edgeCenters.map(n => Math.abs(n - memLoc)));
    const dist = halfRootDist + diffToEdge;
    // this.print(`smallest root ${nextCorner} mem ${memLoc} root ${root} edgeCenters ${edgeCenters} diffToEdge
    // ${diffToEdge} dist ${dist}`);
    return dist;
  }

  part2(): number | string | undefined {
    const limit = this.readInputInts()[0];
    const values = new HashMap<Point, number>();
    values.set(new Point(0, 0), 1);

    let sum = 0;
    let currSq = 2;
    while (currSq < 1000) {
      const currPoint = this.getPoint(currSq);
      const allPoints: Point[] = this.getAllPoints(currPoint);
      sum = allPoints.map(p => values.get(p) || 0).reduce((p, c) => p + c);
      if (sum > limit) {
        return sum;
      }

      values.set(currPoint, sum);
      currSq += 1;
    }

    return 0;
  }

  getPoint(curr: number): Point {
    const halfRoot = Math.floor(Math.ceil(Math.sqrt(curr)) / 2);
    const root = halfRoot * 2 + 1;
    const se = root * root;
    const halfRootDist = Math.floor(root / 2);
    const south = se - halfRootDist;
    const west = se - (root - 1) - halfRootDist;
    const north = se - (root - 1) * 2 - halfRootDist;
    const east = se - (root - 1) * 3 - halfRootDist;
    const sw = se - (root - 1);
    const nw = se - (root - 1) * 2;
    const ne = se - (root - 1) * 3;
    const edgeCenters = [south, west, north, east];
    const diffToEdge = Math.min(...edgeCenters.map(n => Math.abs(n - curr)));

    let x;
    let y;
    if (curr <= ne) {
      x = halfRootDist;
      y = curr < east ? diffToEdge : -diffToEdge;
    } else if (curr < nw) {
      x = curr < north ? diffToEdge : -diffToEdge;
      y = -halfRootDist;
    } else if (curr < sw) {
      x = -halfRootDist;
      y = curr < west ? -diffToEdge : diffToEdge;
    } else {
      x = curr < south ? -diffToEdge : diffToEdge;
      y = halfRootDist;
    }

    return new Point(x, y);
  }

  getAllPoints(center: Point): Point[] {
    const allPoints: Point[] = [];
    [-1, 0, 1].forEach(i => {
      [-1, 0, 1].forEach(j => {
        if (!(i === 0 && j === 0)) {
          allPoints.push(new Point(i + center.x, j + center.y));
        }
      });
    });

    return allPoints;
  }
}

(new Day3()).run();
