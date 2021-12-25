import { Solution } from "../solution";

class Day1 extends Solution {
  constructor(example?: number) {
    super(1, 2021, example);
  }

  part1(): number | string {
    const inputs = this.readInputInts();
    let numIncrease = 0;
    for (let i = 0, j = 1; j < inputs.length; i++, j++) {
      if (inputs[i] < inputs[j]) {
        numIncrease += 1;
      }
    }

    return numIncrease;
  }

  part2(): number | string {
    const inputs = this.readInputInts();
    let numIncrease = 0;
    for (let i = 0, j = 3; j < inputs.length; i++, j++) {
      if (inputs[i] < inputs[j]) {
        numIncrease += 1;
      }
    }

    return numIncrease;
  }
}

(new Day1()).run();
