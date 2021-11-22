import Solution from "./solution";

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
    return undefined;
  }
}

(new Day3()).run();
