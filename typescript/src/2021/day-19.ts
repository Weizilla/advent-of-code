import { Solution } from "../solution";
import { HashMap, HashSet, Tuple } from "../collections";
import { print, range } from "../utils";

const ROTATIONS = [
  [Tuple.z(0)],
  [Tuple.z(1)],
  [Tuple.z(2)],
  [Tuple.z(3)],

  [Tuple.y(1), Tuple.x(0)],
  [Tuple.y(1), Tuple.x(1)],
  [Tuple.y(1), Tuple.x(2)],
  [Tuple.y(1), Tuple.x(3)],

  [Tuple.y(2), Tuple.z(0)],
  [Tuple.y(2), Tuple.z(1)],
  [Tuple.y(2), Tuple.z(2)],
  [Tuple.y(2), Tuple.z(3)],

  [Tuple.y(3), Tuple.x(0)],
  [Tuple.y(3), Tuple.x(1)],
  [Tuple.y(3), Tuple.x(2)],
  [Tuple.y(3), Tuple.x(3)],

  [Tuple.x(1), Tuple.y(0)],
  [Tuple.x(1), Tuple.y(1)],
  [Tuple.x(1), Tuple.y(2)],
  [Tuple.x(1), Tuple.y(3)],

  [Tuple.x(3), Tuple.y(0)],
  [Tuple.x(3), Tuple.y(1)],
  [Tuple.x(3), Tuple.y(2)],
  [Tuple.x(3), Tuple.y(3)],
];

class Scanner {
  id: number;
  points: Tuple[];

  constructor(id: number) {
    this.id = id;
    this.points = [];
  }

  toString(): string {
    return `[id=${this.id} ${this.points}]`;
  }
}

class TupleId {
  id: number;
  rotation: Tuple;

  constructor(id: number, rotation: Tuple) {
    this.id = id;
    this.rotation = rotation;
  }

  toString(): string {
    return `${this.id}|${this.rotation}`;
  }
}

class Day19 extends Solution {
  rotations: HashMap<TupleId, Tuple[]>;


  constructor(example?: number, forcePrint: boolean = false) {
    super(19, 2021, example, forcePrint);
    this.rotations = new HashMap<TupleId, Tuple[]>(t => t.toString());
  }

  part1(): number | string {
    const scanners = this.readScanners();
    return this.runPart1c(scanners)[0];
  }

  runPart1c(scanners: Scanner[]): [number, Tuple[]] {
    const merged = new Map<number, Tuple[]>();
    merged.set(scanners[0].id, scanners[0].points);
    let toMerge = range(scanners.length, 1);

    const translations: Tuple[] = [];
    const compared = new HashSet<string>(s => s);

    while (toMerge.length > 0) {
      const thisRound = [...toMerge];
      toMerge = [];

      for (const toMergeId of thisRound) {
        const scanner = scanners[toMergeId];

        let didMerge = false;
        for (const mergedId of merged.keys()) {

          const comparedKey = `${mergedId}:${scanner.id}`;
          if (!compared.has(comparedKey)) {
            compared.add(comparedKey);
            const mergedTuples = merged.get(mergedId)!;
            const [points, translation] = this.calcOverlapTuples(mergedTuples, scanner);

            if (points.length > 0) {
              merged.set(toMergeId, points);
              didMerge = true;
              if (translation) {
                translations.push(translation);
              }
              break;
            }
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
      const allTuples = new HashSet<Tuple>(t => t.toString());
      Array.from(merged.values()).forEach(p => allTuples.add(...p));
      return [allTuples.size(), translations];
    } else {
      print(`Didn't merge all scanners ${merged.size} ${scanners.length}`, 0, "red");
      return [0, translations];
    }
  }

  private calcOverlapTuples(baseTuples: Tuple[], scanner: Scanner): [Tuple[], Tuple | null] {
    for (const baseTuple of baseTuples) {
      for (const rotMatrix of ROTATIONS) {
        const rotation = this.rotate(scanner, rotMatrix);
        for (const rotationTuple of rotation) {
          const [rotationTranslated, translation] = this.translate(baseTuple, rotationTuple, rotation);
          const numMatch = this.numMatch(baseTuples, rotationTranslated);
          if (numMatch >= 12) {
            return [rotationTranslated, translation];
          }
        }
      }
    }
    return [[], null];
  }

  private numMatch(pointsA: Iterable<Tuple>, pointsB: Iterable<Tuple>): number {
    const a = new HashSet<Tuple>(t => t.toString(), pointsA);
    const b = new HashSet<Tuple>(t => t.toString(), pointsB);
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
        const point = new Tuple(x, y, z);
        curr?.points.push(point);
      }
    });

    return scanners;
  }

  private rotate(scanner: Scanner, rotations: Tuple[]): Tuple[] {
    let {points} = scanner;
    for (const rotation of rotations) {
      points = points.map(p => this.rotateTuple(p, rotation));
    }
    return points;
  }

  private rotateTuple(point: Tuple, rotation: Tuple): Tuple {
    const rRad = rotation.scale(Math.PI / 2);

    const rx: [Tuple, Tuple, Tuple] = [
      new Tuple(1, 0, 0),
      new Tuple(0, Math.cos(rRad.x), -Math.sin(rRad.x)),
      new Tuple(0, Math.sin(rRad.x), Math.cos(rRad.x)),
    ];
    const ry: [Tuple, Tuple, Tuple] = [
      new Tuple(Math.cos(rRad.y), 0, Math.sin(rRad.y)),
      new Tuple(0, 1, 0),
      new Tuple(-Math.sin(rRad.y), 0, Math.cos(rRad.y)),
    ];
    const rz: [Tuple, Tuple, Tuple] = [
      new Tuple(Math.cos(rRad.z), -Math.sin(rRad.z), 0),
      new Tuple(Math.sin(rRad.z), Math.cos(rRad.z), 0),
      new Tuple(0, 0, 1),
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

  private translate(base: Tuple, reference: Tuple, points: Tuple[]): [Tuple[], Tuple] {
    const t = base.sum(reference.scale(-1));
    const newTuples = points.map(p => p.sum(t));
    return [newTuples, t];
  }

  part2(): number | string {
    const scanners = this.readScanners();
    const translations = this.runPart1c(scanners)[1];

    let maxDistance = 0;

    for (let i = 0; i < translations.length; i++) {
      for (let j = i + 1; j < translations.length; j++) {
        maxDistance = Math.max(maxDistance, translations[i].distance(translations[j]));
      }
    }
    return maxDistance;
  }
}

(new Day19(undefined, true)).run();
