import { Solution } from "../solution";
import {print, range} from "../utils";

class Day6 extends Solution {
  constructor(example?: number) {
    super(6, 2021, example);
  }

  part1(): number | string | undefined {
    const initial = [3, 4, 3, 1, 2];
    const fishes = this.makeFishes(initial, 4);
    return fishes;
    print(this.makeFish(3, 0, 18)); // 4, 11, 18
    // print(this.makeFish(0, 4, 18)); // 13
    // print(this.makeFish(8, 4, 18));
    // print(this.makeFish(8, 11,18));
    // print(this.makeFish(8, 18,18));

    // const num = this.makeFishes(initial, 18);
    // return num;
    return 0;
  }

  makeFish(initial: number, startDayInput: number, endDay: number): number[] {
    // return start days
    const startDay = startDayInput - (8 - initial);
    const numFish = Math.trunc((endDay - startDay) / 7);
    const newFish = range(numFish, 1, true).map(n => (n * 7) + startDay + 2).filter(n => n > 0 && n <= endDay);
    // print(`${initial} ${startDayInput} ${startDay} ${numFish} new ${newFish}`);
    return newFish;
  }

  makeFishes(initial: number[], days: number): number {
    if (initial.length === 0) {
      return 0;
    }

    let newFishes = initial.flatMap(n => this.makeFish(n, 0, days));
    print(newFishes);
    let numFishes = newFishes.length;
    while (newFishes.length > 0) {
      const moreFish = newFishes.flatMap(n => this.makeFish(8, n, days));
      numFishes += moreFish.length;
      newFishes = moreFish;
    }
    return numFishes + initial.length;
  }

  part2(): number | string | undefined {
    return undefined;
  }
}

(new Day6(1)).run();
