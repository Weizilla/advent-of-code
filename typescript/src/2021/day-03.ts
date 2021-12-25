import { Solution } from "../solution";
import { print } from "../utils";

class Day3 extends Solution {
  constructor(example?: number) {
    super(3, 2021, example);
  }

  part1(): number | string {
    const inputs = this.readInput();
    const sums = new Array(inputs[0].length).fill(0);
    for (const line of inputs) {
      for (let i = 0; i < line.length; i++) {
        const digit = line[i];
        if (parseInt(digit, 10) === 1) {
          sums[i] += 1;
        }
      }
    }

    const final = [];
    for (const sum of sums) {
      if (sum > inputs.length / 2) {
        final.push(1);
      } else {
        final.push(0);
      }
    }

    const gamma = parseInt(final.join(""), 2);
    const epsilon = gamma ^ parseInt(new Array(inputs[0].length).fill(1).join(""), 2);
    print(`gamma ${gamma} epsilon ${epsilon}`);
    return gamma * epsilon;
  }

  part2(): number | string {
    const inputs = this.readInput();
    const o2 = this.part2Helper(inputs, ["1", "1", "0"]);
    const co2 = this.part2Helper(inputs, ["0", "0", "1"]);
    print(`o2 ${o2} co2 ${co2}`);
    return o2 * co2;
  }

  part2Helper(inputs: string[], checks: [string, string, string]): number {
    const numLen = inputs[0].length;
    let allNums = [...inputs];
    for (let i = 0; i < numLen; i++) {
      let sum = 0;
      for (const num of allNums) {
        if (parseInt(num[i], 10) === 1) {
          sum += 1;
        }
      }

      if (allNums.length > 1) {
        if (sum > allNums.length / 2) {
          allNums = [...allNums].filter(n => n[i] === checks[0]);
        } else if (sum === allNums.length / 2) {
          allNums = [...allNums].filter(n => n[i] === checks[1]);
        } else {
          allNums = [...allNums].filter(n => n[i] === checks[2]);
        }
      }
    }

    return parseInt(allNums[0], 2);
  }
}

(new Day3()).run();
