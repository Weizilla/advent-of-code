import { Solution } from "../solution";

class Day2 extends Solution {
  constructor(example?: number) {
    super(2, 2017, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const allDiffs = inputs.map(line => {
      const nums = line.split(/[\s]/).map(n => parseInt(n, 10));
      return Math.max(...nums) - Math.min(...nums);
    });
    return allDiffs.reduce((prev, curr) => prev + curr);
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const allDiffs = inputs.map(line => {
      const nums = line.split(/[\s]/).map(n => parseInt(n, 10));
      for (let i = 0; i < nums.length; i++) {
        for (let j = i + 1; j < nums.length; j++) {
          const bigNum = Math.max(nums[i], nums[j]);
          const littleNum = Math.min(nums[i], nums[j]);
          if (bigNum % littleNum === 0) {
            return bigNum / littleNum;
          }
        }
      }
      return 0;
    });
    return allDiffs.reduce((prev, curr) => prev + curr);
  }
}

(new Day2()).run();
