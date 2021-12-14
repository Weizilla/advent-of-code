import { Solution } from "../solution";
import { Counter } from "../collections";

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
    const counter = new Counter();
    polymer.split("").forEach(p => {
      counter.count(p);
    });

    const maxNum = counter.maxCount();
    const minNum = counter.minCount();

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
    const polymer = inputs[0].split("");
    let pairs = this.calcPairs(polymer);

    this.printPairs(`step 0`, pairs);

    for (let i = 0; i < 40; i++) {
      pairs = this.runStep2b(pairs, rules);
      this.printPairs(`step act ${i + 1}`, pairs);
    }

    return this.calcCounts2b(polymer, pairs);
  }

  private calcPairs(polymer: string[]): Map<string, number> {
    const pairs = new Map<string, number>();
    for (let i = 0; i < polymer.length - 1; i++) {
      const pair = `${polymer[i]}${polymer[i + 1]}`;
      const count = pairs.get(pair) || 0;
      pairs.set(pair, count + 1);
    }
    return pairs;
  }

  private runStep2b(pairs: Map<string, number>, rules: Map<string, string>): Map<string, number> {
    const newPairs = new Map<string, number>();

    for (const pair of pairs.keys()) {
      if (rules.has(pair)) {
        const insert = rules.get(pair);
        const count = pairs.get(pair)!;

        const [p1, p2] = [...pair.split("")];
        const firstHalf = `${p1}${insert}`;
        const firstHalfCount = newPairs.get(firstHalf) || 0;
        newPairs.set(firstHalf, firstHalfCount + count);

        const lastHalf = `${insert}${p2}`;
        const lastHalfCount = newPairs.get(lastHalf) || 0;
        newPairs.set(lastHalf, lastHalfCount + count);
      } else {
        newPairs.set(pair, pairs.get(pair)!);
      }
    }
    return newPairs;
  }

  private calcCounts2b(polymer: string[], pairs: Map<string, number>): number {
    const counter = new Counter();

    for (const pair of pairs.keys()) {
      const [p1, p2] = [...pair.split("")];
      counter.add(p1, pairs.get(pair)!);
      counter.add(p2, pairs.get(pair)!);
    }

    counter.count(polymer[0]);
    counter.count(polymer[polymer.length - 1]);

    const maxNum = counter.maxCount();
    const minNum = counter.minCount();

    return (maxNum - minNum) / 2;
  }

  printPairs(message: string, pairs: Map<string, number>) {
    let output = `${message} `;
    const keys = [...pairs.keys()].sort();
    keys.forEach(k => {
      output += `${k} ${pairs.get(k)!} `;
    });
    this.print(output);
  }
}

(new Day14()).run();
