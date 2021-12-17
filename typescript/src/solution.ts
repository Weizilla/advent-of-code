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

  print(input: any, padding: number = 0, color: string = "black") {
    if (this.example !== undefined) {
      print(input, padding, color);
    }
  }

  run() {
    let start = Date.now();
    let result = this.part2();
    if (result !== undefined) {
      const duration = Math.ceil((Date.now() - start) / 1000);
      print(`Day ${this.day} Part 2 in ${duration} s`, 0, "blue");
      print(result, 0, "red");
    } else {
      start = Date.now();
      result = this.part1();
      if (result !== undefined) {
        const duration = Math.ceil((Date.now() - start) / 1000);
        print(`Day ${this.day} Part 1 ${duration} s`, 0, "blue");
        print(result, 0, "red");
      } else {
        print(`Day ${this.day} not implemented`, 0, "blue");
      }
    }
  }
}

export { Solution };
