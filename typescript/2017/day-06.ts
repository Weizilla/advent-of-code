import Solution from "./solution";
import { HashMap } from "./collections";

class Day6 extends Solution {
  constructor(example?: number) {
    super(6, example);
  }

  part1(): number | string | undefined {
    const banks = this.readInput()[0].split(/[\s]/).map(s => parseInt(s, 10));
    const seen = new HashMap<number[], number[]>();

    while (seen.get(banks) === undefined) {
      seen.set(banks, banks);

      let max = 0;
      let maxIndex = 0;
      for (let i = 0; i < banks.length; i++) {
        if (max < banks[i]) {
          max = banks[i];
          maxIndex = i;
        }
      }

      let numToDistribute = banks[maxIndex];
      banks[maxIndex] = 0;
      let distIndex = maxIndex + 1;
      while (numToDistribute > 0) {
        if (distIndex === banks.length) {
          distIndex = 0;
        }

        banks[distIndex] += 1;
        numToDistribute -= 1;
        distIndex += 1;
      }
    }

    return seen.size();
  }

  part2(): number | string | undefined {
    const banks = this.readInput()[0].split(/[\s]/).map(s => parseInt(s, 10));
    const seen = new HashMap<number[], number>();

    let numLoop = 0;
    while (seen.get(banks) === undefined) {
      seen.set(banks, numLoop);

      let max = 0;
      let maxIndex = 0;
      for (let i = 0; i < banks.length; i++) {
        if (max < banks[i]) {
          max = banks[i];
          maxIndex = i;
        }
      }

      let numToDistribute = banks[maxIndex];
      banks[maxIndex] = 0;
      let distIndex = maxIndex + 1;
      while (numToDistribute > 0) {
        if (distIndex === banks.length) {
          distIndex = 0;
        }

        banks[distIndex] += 1;
        numToDistribute -= 1;
        distIndex += 1;
      }

      numLoop += 1;
    }

    return numLoop - (seen.get(banks) || 0);
  }
}

(new Day6()).run();
