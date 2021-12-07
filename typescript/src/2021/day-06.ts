import { Solution } from "../solution";
import { range } from "../utils";

class Day6 extends Solution {
  constructor(example?: number) {
    super(6, 2021, example);
  }

  part1(): number | string | undefined {
    const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const fishes = this.makeFishes(initial, 80);
    return fishes;
  }

  makeFish(initial: number, startDayInput: number, endDay: number): number[] {
    // returns start days
    const startDay = startDayInput - (8 - initial);
    const numFish = Math.trunc((endDay - startDay) / 7);
    if (numFish === 0) {
      return [];
    }
    const newFish = range(numFish, 1, true).map(n => (n * 7) + startDay + 2).filter(n => n > 0 && n <= endDay);
    // print(`init=${initial} start=${startDayInput} real_start=${startDay} num=${numFish}
    // new=${JSON.stringify(newFish)}`);
    return newFish;
  }

  makeFishes(initial: number[], days: number): number {
    if (initial.length === 0) {
      return 0;
    }

    let newFishes = initial.flatMap(n => this.makeFish(n, 0, days));
    // print(newFishes);
    let numFishes = newFishes.length;
    while (newFishes.length > 0) {
      const moreFish = newFishes.flatMap(n => this.makeFish(8, n, days));
      numFishes += moreFish.length;
      newFishes = moreFish;
    }
    return numFishes + initial.length;
  }

  part2(): number | string | undefined {
    const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const fishes = this.makeFishes(initial, 256);
    return fishes;
  }
}

(new Day6(1)).run();
