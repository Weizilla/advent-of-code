import { Solution } from "../solution";
import { sum } from "../utils";

class Day8 extends Solution {
  constructor(example?: number) {
    super(8, 2021, example);
  }

  part1(): number | string {
    const input = this.readInput();
    const allOutputs = input
      .map(this.parseInput)
      .flatMap(n => n[1])
      .filter(n => [2, 3, 4, 7].indexOf(n.length) > -1);
    return allOutputs.length;
  }

  parseInput(line: string): [string[], string[]] {
    const splits = line.split("|").map(n => n.trim());
    const inputs = splits[0].split(" ").map(n => n.split("").sort().join(""));
    const outputs = splits[1].split(" ").map(n => n.split("").sort().join(""));
    return [inputs, outputs];
  }

  part2(): number | string {
    const input = this.readInput();
    const allSum = sum(input
      .map(this.parseInput)
      .map(v => this.decodeLine(...v)));
    return allSum;
  }

  decodeLine(inputs: string[], output: string[]): number {
    const top = this.filterInputs(inputs, 1, [], 2, 3);
    const topLeft = this.filterInputs(inputs, 2, [], 4, 5);
    const middle = this.filterInputs(inputs, 4, [], 4, 5);
    const bottomLeft = this.filterInputs(inputs, 1, [], 4, 5);
    const topRight = this.filterInputs(inputs, 2, [middle, bottomLeft], 6);
    const bottomRight = this.filterInputs(inputs, 1, [topRight], 2);
    const bottom = this.filterInputs(inputs, 1, [top, topLeft, topRight, middle, bottomLeft, bottomRight], 7);

    // print(`top ${top}
    // topLeft ${topLeft} topRight ${topRight}
    // middle ${middle}
    // bottomLeft ${bottomLeft} bottomRight ${bottomRight}
    // bottom ${bottom}`);

    const nums = new Map<string, number>();
    nums.set([top, topLeft, topRight, bottomLeft, bottomRight, bottom].sort().join(""), 0);
    nums.set([topRight, bottomRight].sort().join(""), 1);
    nums.set([top, topRight, middle, bottomLeft, bottom].sort().join(""), 2);
    nums.set([top, topRight, middle, bottomRight, bottom].sort().join(""), 3);
    nums.set([topLeft, topRight, middle, bottomRight].sort().join(""), 4);
    nums.set([top, topLeft, middle, bottomRight, bottom].sort().join(""), 5);
    nums.set([top, topLeft, middle, bottomLeft, bottomRight, bottom].sort().join(""), 6);
    nums.set([top, topRight, bottomRight].sort().join(""), 7);
    nums.set([top, topLeft, topRight, middle, bottomLeft, bottomRight, bottom].sort().join(""), 8);
    nums.set([top, topLeft, topRight, middle, bottomRight, bottom].sort().join(""), 9);

    return parseInt(output.map(n => {
      if (nums.get(n) !== undefined) {
        return nums.get(n)!.toString();
      } else {
        throw Error(`Not found ${n} ${JSON.stringify([...nums.entries()])}`);
      }
    }).join(""), 10);

    return 0;
  }

  filterInputs(input: string[], filterLength: number, excludes: string[], ...combineLengths: number[]): string {
    const allLetters = input
      .filter(s => combineLengths.indexOf(s.length) > -1)
      .flatMap(s => s.split(""))
      .filter(s => excludes.indexOf(s) === -1);

    const counts = new Map<string, number>();
    allLetters.forEach(l => {
      const currCount = counts.get(l) || 0;
      counts.set(l, currCount + 1);
    });

    const desiredLens = [...counts.entries()].filter(([_, n]) => n === filterLength);
    if (desiredLens.length !== 1) {
      throw Error(`More tha one ${desiredLens} ${filterLength}`);
    }

    return desiredLens[0][0];
  }
}

(new Day8()).run();
