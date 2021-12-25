import { Solution } from "../solution";
import { range, sum } from "../utils";

class Day6 extends Solution {
  fishOutput: Map<number, number>;

  constructor(example?: number) {
    super(6, 2021, example);
    this.fishOutput = new Map<number, number>();
  }

  part1(): number | string {
    const endDay = 80;
    const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const num = sum(initial.map(n => n - 8).map(n => this.countFish(n, endDay)));
    return num;
  }

  countFish(startDay: number, endDay: number): number {
    const cached = this.fishOutput.get(startDay);
    if (cached !== undefined) {
      return cached;
    }

    const numFish = Math.trunc((endDay - startDay) / 7);
    if (numFish === 0) {
      this.fishOutput.set(startDay, 1);
      return 1;
    }

    const newFish = range(numFish, 1, true)
      .map(n => (n * 7) + startDay + 2)
      .filter(n => n > 0 && n <= endDay);

    if (newFish.length === 0) {
      this.fishOutput.set(startDay, 1);
      return 1;
    }

    const numNewFish = sum(newFish.map(n => this.countFish(n, endDay)));
    const result = 1 + numNewFish;
    this.fishOutput.set(startDay, result);

    return result;
  }

  part2(): number | string {
    const endDay = 256;
    const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const num = sum(initial.map(n => n - 8).map(n => this.countFish(n, endDay)));
    return num;
  }
}

(new Day6()).run();
