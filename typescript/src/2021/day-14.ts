import { Solution } from "../solution";

class Day14 extends Solution {
  constructor(example?: number) {
    super(14, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const rules = this.readRules(inputs);

    let polymer = inputs[0];
    for (let i = 0; i < 10; i++) {
      polymer = this.runStep(polymer, rules);
    }
    return this.calcMinMaxDiff(polymer);
  }

  private readRules(inputs: string[]) {
    const rules = new Map<string, string>();

    for (let i = 1; i < inputs.length; i++) {
      const [input, _, output] = [...inputs[i].split(" ")];
      rules.set(input, output);
    }
    return rules;
  }

  private calcMinMaxDiff(polymer: string) {
    const counts = new Map<string, number>();
    polymer.split("").forEach(p => {
      const count = counts.get(p) || 0;
      counts.set(p, count + 1);
    });

    const maxNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n2 - n1)[0][1];
    const minNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n1 - n2)[0][1];

    this.print(`${minNum} ${maxNum}`);

    return maxNum - minNum;
  }

  runStep(input: string, rules: Map<string, string>): string {
    let output = "";

    for (let i = 0; i < input.length; i++) {
      if ((i + 1) < input.length) {
        const pair = `${input[i]}${input[i + 1]}`;
        if (rules.has(pair)) {
          const added = `${input[i]}${rules.get(pair)!}`;
          output += added;
          // this.print(`${pair} ${added} ${output}`);
        } else {
          output += input[i];
          // this.print(`${input[i]} ${input[i]} ${output}`);
        }
      } else {
        output += input[i];
        // this.print(`${input[i]} ${input[i]} ${output}`);
      }
    }

    return output;
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const rules = this.readRules(inputs);

    let polymer = inputs[0];
    for (let i = 0; i < 40; i++) {
      polymer = this.runStep(polymer, rules);
    }
    return this.calcMinMaxDiff(polymer);
  }
}

(new Day14()).run();
