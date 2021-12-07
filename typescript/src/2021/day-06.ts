import { Solution } from "../solution";
import { print, range, sum } from "../utils";

class Day6 extends Solution {
  fishOutput: Map<number, number>;

  constructor(example?: number) {
    super(6, 2021, example);
    this.fishOutput = new Map<number, number>();
  }

  part1(): number | string | undefined {
    const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    const fishes = this.countFishes(initial, 80);
    return fishes;
  }

  makeFish(initial: number, startDayInput: number, endDay: number): number[] {
    // if (initial === 8) {
    //   const cached = this.fishOutput.get(startDayInput);
    //   if (cached) {
    //     return cached;
    //   }
    // }
    // returns start days
    const startDay = startDayInput - (8 - initial);
    const numFish = Math.trunc((endDay - startDay) / 7);
    if (numFish === 0) {
      return [];
    }
    const newFish = range(numFish, 1, true).map(n => (n * 7) + startDay + 2).filter(n => n > 0 && n <= endDay);
    // print(`init=${initial} start=${startDayInput} real_start=${startDay} num=${numFish}
    // new=${JSON.stringify(newFish)}`);
    // if (initial === 8) {
    //   this.fishOutput.set(startDayInput, newFish);
    // }
    return newFish;
  }

  countFishes(initial: number[], days: number): number {
    let newFishes = initial.flatMap(n => this.makeFish(n, 0, days)).sort((a, b) => a - b);
    print(newFishes);
    let numFishes = newFishes.length;
    while (newFishes.length > 0) {
      const moreFish = newFishes.flatMap(n => this.makeFish(8, n, days)).sort((a, b) => a - b);
      numFishes += moreFish.length;
      newFishes = moreFish;
      print(` num ${numFishes} cached ${[...this.fishOutput.keys()]}`);
    }
    return numFishes + initial.length;
  }

  part2(): number | string | undefined {
    const initial = [3];
    const numDays = 100;
    let newFishes = initial.flatMap(n => this.makeFish(n, 0, numDays)).sort((a, b) => a - b);

    const counts = newFishes.map(n => this.countFish(n, numDays));
    print(`counts ${counts} ${sum(counts) + initial.length}`);

    // const initial = this.readInput()[0].split(",").map(n => parseInt(n, 10));
    // const fishes = this.makeFishes2(initial, 256);
    // return fishes;
    print(this.countFish(2, 18));
    return sum(counts) + initial.length;
  }

  countFish(startDay: number, days: number): number {
    const cached = this.fishOutput.get(startDay);
    if (cached) {
      return cached;
    }
    let newFishes = this.makeFish(8, startDay, days).sort((a, b) => a - b);
    print(newFishes);
    let numFishes = newFishes.length;
    while (newFishes.length > 0) {
      const moreFish = sum(newFishes.map(n => this.countFish(n, days)));
      numFishes += moreFish;
      print(` num ${numFishes} cached ${[...this.fishOutput.keys()]}`);
    }
    const result = numFishes + 1;
    this.fishOutput.set(startDay, result);

    return result;
  }
}

(new Day6(1)).run();
