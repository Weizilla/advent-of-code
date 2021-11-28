import { readFileSync } from "fs";
import path from "path";
import { print } from "./utils";

class Solution {
  day: number;
  year: number;
  example?: number;

  constructor(day: number, year: number, example?: number) {
    this.day = day;
    this.year = year;
    this.example = example;
  }

  part1(): number | string | undefined {
    return undefined;
  }

  part2(): number | string | undefined {
    return undefined;
  }

  readInput(): string[] {
    const day = this.day.toString().padStart(2, "0");
    let inputFilename;
    if (this.example !== undefined) {
      inputFilename = `inputs/day-${day}-example-${this.example}.txt`;
    } else {
      inputFilename = `inputs/day-${day}-input.txt`;
    }
    const inputs = readFileSync(path.join(__dirname, this.year.toString(), inputFilename), "utf-8");
    return inputs.split("\n")
      .filter((v) => v.trim().length > 0);
  }

  readInputInts(): number[] {
    return this.readInput().map(n => parseInt(n, 10));
  }

  run() {
    let result = this.part2();
    if (result !== undefined) {
      print(`Day ${this.day} Part 2`, "blue");
      print(result, "red");
    } else {
      result = this.part1();
      if (result !== undefined) {
        print(`Day ${this.day} Part 1`, "blue");
        print(result, "red");
      } else {
        print(`Day ${this.day} not implemented`, "blue");
      }
    }
  }
}

export { Solution };