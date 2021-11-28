import { Solution } from "../solution";

class Day1 extends Solution {
  constructor(example?: number) {
    super(1, 2017, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput()[0].split("").map((i) => parseInt(i, 10));
    let totalSum = 0;
    for (let i = 0; i < inputs.length; i++) {
      if (i + 1 < inputs.length && inputs[i] === inputs[i + 1]) {
        totalSum += inputs[i];
      }
    }

    if (inputs[0] === inputs[inputs.length - 1]) {
      totalSum += inputs[0];
    }

    return totalSum;
  }

  part2(): number | string | undefined {
    const inputs = this.readInput()[0].split("").map((i) => parseInt(i, 10));
    let totalSum = 0;
    for (let i = 0; i < inputs.length; i++) {
      const half = inputs.length / 2;
      let nextIndex;
      if (i < half) {
        nextIndex = i + half;
      } else {
        nextIndex = i - half;
      }
      if (inputs[i] === inputs[nextIndex]) {
        totalSum += inputs[i];
      }
    }

    return totalSum;
  }
}

(new Day1()).run();
