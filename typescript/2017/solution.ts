import chalk from "chalk";
import { readFileSync } from "fs";
import path from "path";

class Solution {
  day: number;
  example?: number;

  constructor(day: number, example?: number) {
    this.day = day;
    this.example = example;
  }

  part1(): number | string | undefined {
    return undefined;
  }

  part2(): number | string | undefined {
    return undefined;
  }

  print(input: any, color: string = "black") {
    console.log(chalk.keyword(color)("%s"), input);
  }

  readInput(): string[] {
    const day = this.day.toString().padStart(2, "0");
    let inputFilename;
    if (this.example !== undefined) {
      inputFilename = `inputs/day-${day}-example-${this.example}.txt`;
    } else {
      inputFilename = `inputs/day-${day}-input.txt`;
    }
    const inputs = readFileSync(path.join(__dirname, inputFilename), "utf-8");
    return inputs.split("\n")
      .filter((v) => v.trim().length > 0);
  }

  readInputInts(): number[] {
    return this.readInput().map(n => parseInt(n, 10));
  }

  run() {
    let result = this.part2();
    if (result !== undefined) {
      this.print(`Day ${this.day} Part 2`, "blue");
      this.print(result, "red");
    } else {
      result = this.part1();
      if (result !== undefined) {
        this.print(`Day ${this.day} Part 1`, "blue");
        this.print(result, "red");
      } else {
        this.print(`Day ${this.day} not implemented`, "blue");
      }
    }
  }
}

export default Solution;
