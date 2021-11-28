import { Solution } from "../solution";

class Day5 extends Solution {
  constructor(example?: number) {
    super(5, 2017, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInputInts();
    let currIndex = 0;
    let numSteps = 0;
    while (currIndex >= 0 && currIndex < inputs.length) {
      const currValue = inputs[currIndex];
      inputs[currIndex] += 1;
      currIndex += currValue;
      numSteps += 1;
    }
    return numSteps;
  }

  part2(): number | string | undefined {
    const inputs = this.readInputInts();
    let currIndex = 0;
    let numSteps = 0;
    while (currIndex >= 0 && currIndex < inputs.length) {
      const currValue = inputs[currIndex];
      if (currValue >= 3) {
        inputs[currIndex] -= 1;
      } else {
        inputs[currIndex] += 1;
      }
      currIndex += currValue;
      numSteps += 1;
    }
    return numSteps;
  }
}

(new Day5()).run();
