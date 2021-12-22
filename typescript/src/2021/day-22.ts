import { Solution } from "../solution";
import { HashSet, NumGrid, Point } from "../collections";
import { sum } from "../utils";

class Cube {
  on: boolean;
  minX: number;
  maxX: number;
  minY: number;
  maxY: number;
  minZ: number;
  maxZ: number;
  allBounds: [number, number, number, number, number, number];

  constructor(on: boolean, minX: number, maxX: number, minY: number, maxY: number, minZ: number, maxZ: number) {
    this.on = on;
    this.minX = minX;
    this.maxX = maxX;
    this.minY = minY;
    this.maxY = maxY;
    this.minZ = minZ;
    this.maxZ = maxZ;
    this.allBounds = [minX, maxX, minY, maxY, minZ, maxZ];
  }

  allPoints(): Point[] {
    const points = [];
    for (let x = this.minX; x < this.maxX; x++) {
      for (let y = this.minY; y < this.maxY; y++) {
        for (let z = this.minZ; z < this.maxZ; z++) {
          points.push(new Point(x, y, z));
        }
      }
    }
    return points;
  }

  size(): number {
    return (this.maxX - this.minX) * (this.maxY - this.minY) * (this.maxZ - this.minZ);
  }

  toString(): string {
    return `on=${this.on} x=${this.minX}..${this.maxX} y=${this.minY}..${this.maxY} z=${this.minZ}..${this.maxZ}`;
  }
}

class Day22 extends Solution {
  constructor(example?: number) {
    super(22, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const inputCubes = inputs.map(i => this.parseInput(i)).filter(c => c.allBounds.filter(p => Math.abs(p) <= 51).length === 6);
    // this.print(cubes.join("\n"));

    // return this.brute(inputCubes);
    return this.smart(inputCubes);
  }

  brute(cubes: Cube[]) {
    const grid = new NumGrid();
    cubes.forEach(cube => {
      cube.allPoints().forEach(p => grid.set(p, cube.on ? 1 : 0));
    });

    return grid.values().filter(v => v === 1).length;
  }

  test() {
    const c = new Cube(true, 0, 10, -1, 11, 0, 1);
    const c2 = new Cube(true, 1, 9, 2, 14, 0, 1);
    const sub = this.subtract(c, c2);

    const numGrid = new NumGrid();
    c.allPoints().forEach(p => numGrid.set(p, 1));
    this.print(numGrid.toString());

    c2.allPoints().forEach(p => numGrid.set(p, 2));
    this.print(numGrid.toString());

    sub.forEach(s => s.allPoints().forEach(p => numGrid.set(p, 3)));
    this.print(numGrid.toString());
  }

  merge(cubes: Cube[]): Cube[] {
    const drop: Cube[] = [];

    cubes.sort((c1, c2) => c2.size() - c1.size());

    for (let i = 0; i < cubes.length; i++) {
      for (let j = i + 1; j < cubes.length; j++) {
        const bigger = cubes[i];
        const smaller = cubes[j];
        if (this.overlap(bigger, smaller)) {
          drop.push(smaller);
        }
      }
    }

    const results = cubes.filter(c => drop.indexOf(c) === -1);
    this.print(`Merge before=${cubes.length} after=${results.length}`);
    return results;
  }

  overlap(a: Cube, b: Cube): boolean {
    return a.minX <= b.minX && b.maxX <= a.maxX
      && a.minY <= b.minY && b.maxY <= a.maxY
      && a.minZ <= b.minZ && b.maxZ <= a.maxZ;
  }

  add(cubes: Cube[], c2: Cube): Cube[] {
    const difference = cubes.flatMap(c => this.subtract(c, c2));
    difference.push(c2);

    const stateSize = this.sumSize(difference);
    const uniqueSize = this.uniqueSize(difference);

    if (stateSize !== uniqueSize) {
      this.print(`difference STATE NOT UNIQUE state=${stateSize} unique=${uniqueSize}`, 3, "red");
    } else {
      this.print(`difference state is unique state=${stateSize} unique=${uniqueSize}`, 3, "green");
    }

    return difference;
  }

  subtract(m: Cube, s: Cube): Cube[] {
    const results: Cube[] = [];

    if (
      (s.maxX < m.minX) || (s.minX > m.maxX)
      || (s.maxY < m.minY) || (s.minY > m.maxY)
      || (s.maxZ < m.minZ) || (s.minZ > m.maxZ)
    ) {
      return [m];
    }

    //        m.minX - s.minX - s.maxX - m.maxX
    // m.minY    1
    //  |
    // s.minY             2
    //  |
    // s.maxY                    3
    //  |
    // m.maxY                             4

    const x1 = (m.minX <= s.minX) ? m.minX : null;
    const x2 = (m.minX <= s.minX) ? s.minX : m.minX;
    const x3 = (s.maxX <= m.maxX) ? s.maxX : m.maxX;
    const x4 = (s.maxX <= m.maxX) ? m.maxX : null;
    const xs: [number | null, number | null][] = [[x1, x2], [x2, x3], [x3, x4]];

    const y1 = (m.minY <= s.minY) ? m.minY : null;
    const y2 = (m.minY <= s.minY) ? s.minY : m.minY;
    const y3 = (s.maxY <= m.maxY) ? s.maxY : m.maxY;
    const y4 = (s.maxY <= m.maxY) ? m.maxY : null;
    const ys: [number | null, number | null][] = [[y1, y2], [y2, y3], [y3, y4]];

    const z1 = (m.minZ <= s.minZ) ? m.minZ : null;
    const z2 = (m.minZ <= s.minZ) ? s.minZ : m.minZ;
    const z3 = (s.maxZ <= m.maxZ) ? s.maxZ : m.maxZ;
    const z4 = (s.maxZ <= m.maxZ) ? m.maxZ : null;
    const zs: [number | null, number | null][] = [[z1, z2], [z2, z3], [z3, z4]];

    for (let x = 0; x < xs.length; x++) {
      const [minX, maxX] = xs[x];
      for (let y = 0; y < ys.length; y++) {
        const [minY, maxY] = ys[y];

        for (let z = 0; z < zs.length; z++) {
          const [minZ, maxZ] = zs[z];

          if (x === 1 && y === 1 && z === 1) {
            continue;
          }

          if (minX !== null && maxX !== null
            && minY !== null && maxY !== null
            && minZ !== null && maxZ !== null
          ) {
            const c = new Cube(true, minX, maxX, minY, maxY, minZ, maxZ);
            // this.print(`sub c ${c}`);
            results.push(c);
          }
        }
      }
    }

    return results;
  }

  parseInput(line: string): Cube {
    const pattern = /([a-z]+) x=([\d-]+)..([\d-]+),y=([\d-]+)..([\d-]+),z=([\d-]+)..([\d-]+)/;
    const match = line.match(pattern)!;
    const allBounds = [...match.slice(2)].map(i => parseInt(i, 10));
    const [minX, maxX, minY, maxY, minZ, maxZ] = allBounds;
    const on = match[1] === "on";
    return new Cube(on, minX, maxX + 1, minY, maxY + 1, minZ, maxZ + 1);
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const inputCubes = inputs.map(i => this.parseInput(i));
    // this.print(cubes.join("\n"));
    return this.smart(inputCubes);
  }

  private smart(inputCubes: Cube[]): number {
    let state: Cube[] = [];

    inputCubes.forEach(inputCube => {
      if (state.length === 0) {
        state.push(inputCube);
      } else if (inputCube.on) {
        const newState = this.add(state, inputCube);
        state = newState;
      } else {
        const newState = state.flatMap(s => this.subtract(s, inputCube));
        state = newState;
      }

      const stateSize = this.sumSize(state);
      const uniqueSize = this.uniqueSize(state);

      if (stateSize !== uniqueSize) {
        this.print(`difference STATE NOT UNIQUE state=${stateSize} unique=${uniqueSize}`, 1, "red");
      } else {
        this.print(`difference state is unique state=${stateSize} unique=${uniqueSize}`, 1, "green");
      }

      state = this.merge(state);

    });

    const stateSize = this.sumSize(state);
    const uniqueSize = this.uniqueSize(state);

    if (stateSize !== uniqueSize) {
      this.print(`difference STATE NOT UNIQUE state=${stateSize} unique=${uniqueSize}`, 0, "red");
    } else {
      this.print(`difference state is unique state=${stateSize} unique=${uniqueSize}`, 0, "green");
    }

    return stateSize;
  }

  uniqueSize(cubes: Cube[]): number {
    const allPoints = new HashSet<Point>();
    cubes.flatMap(c => c.allPoints()).forEach(p => allPoints.add(p));
    return allPoints.size();
  }

  sumSize(cubes: Cube[]): number {
    return sum(cubes.map(c => c.size()));
  }

}

(new Day22()).run();
