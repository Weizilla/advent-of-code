import { Solution } from "../solution";
import { print, range, sum } from "../utils";

class Day7 extends Solution {
  constructor(example?: number) {
    super(7, 2021, example);
  }

  part1(): number | string {
    const locs = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const minLoc = Math.min(...locs);
    const maxLoc = Math.max(...locs);
    const best = range(maxLoc, minLoc)
      .map(n => [n, this.costBruteForce(locs, n)])
      .sort(([_a, b], [_b, d]) => b - d);

    // print(best[0]);

    return best[0][1];
  }

  costBruteForce(locs: number[], final: number): number {
    return sum(locs.map(n => Math.abs(final - n)));
  }

  part2(): number | string {
    const locs = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const minLoc = Math.min(...locs);
    const maxLoc = Math.max(...locs);
    const best = range(maxLoc, minLoc)
      .map(n => [n, this.costBruteForce2(locs, n)])
      .sort(([_a, b], [_b, d]) => b - d);

    // print(best[0]);
    print(this.constMove(16, 5));
    print(this.constMove(1, 5));

    return best[0][1];
  }

  costBruteForce2(locs: number[], final: number): number {
    return sum(locs.map(n => this.constMove(n, final)));
  }

  constMove(start: number, end: number): number {
    const diff = Math.abs(start - end);
    const half = diff / 2;
    return half * (diff + 1);
  }
}

(new Day7()).run();
