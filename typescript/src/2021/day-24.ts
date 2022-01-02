import chalk from "chalk";
import { Solution } from "../solution";
import { range } from "../utils";

class State {
  w: number;
  x: number;
  y: number;
  z: number;

  constructor(w: number = 0, x: number = 0, y: number = 0, z: number = 0) {
    this.w = w;
    this.x = x;
    this.y = y;
    this.z = z;
  }

  set(varName: string, value: number) {
    if (value === undefined) {
      throw Error(`Setting undefined`);
    }
    switch (varName) {
      case "w":
        this.w = value;
        break;
      case "x":
        this.x = value;
        break;
      case "y":
        this.y = value;
        break;
      case "z":
        this.z = value;
        break;
      default:
        throw new Error();
    }
  }

  get(varName: string): number {
    switch (varName) {
      case "w":
        return this.w;
      case "x":
        return this.x;
      case "y":
        return this.y;
      case "z":
        return this.z;
      default:
        throw new Error(`Unknown get ${varName}`);
    }
  }

  getOrParse(varInput: string): number {
    const parsed = parseInt(varInput, 10);
    return Number.isNaN(parsed) ? this.get(varInput) : parsed;
  }

  toString(): string {
    return `{w=${this.w} x=${this.x} y=${this.y} z=${this.z}}`;
  }
}

class Day24 extends Solution {
  constructor(example?: number, forcePrint: boolean = false) {
    super(24, 2021, example, forcePrint);
  }

  part1(): number | string {
    const commands = this.readInput();

    // method 1 - by hand
    this.findDups(commands);
    const finalInput = [3, 9, 4, 9, 4, 1, 9, 5, 7, 9, 9, 9, 7, 9];
    const r = this.runWithInput(finalInput, commands);
    this.print(`Run with input ${finalInput.join("")} result ${r}`);

    // method 2 - brute foce
    const desiredZs = this.runForwards(commands);
    const desiredDigits = this.runBackwards(desiredZs, commands, true);

    return desiredDigits;
  }

  private runWithInput(input: number[], commands: string[], z: number = 0): number {
    // const input = inputString.split("").map(n => parseInt(n, 10));
    const state = this.runProgram(new State(0, 0, 0, z), input, commands);
    this.print(`input=${input.join("")} result=${state}`);
    return state.z;
  }

  private findDups(commands: string[]) {
    let i = 0;
    const output: string[][] = range(18).map(r => []);
    const unique: Set<string>[] = range(18).map(_ => new Set<string>());

    commands.forEach(c => {
      if (c.indexOf("inp") !== -1) {
        i = 0;
      }
      unique[i].add(c);
      if (i === 4 || i === 5 || i === 15) {
        output[i].push(`|${chalk.red(c.padStart(10))}`);
      } else {
        output[i].push(`|${chalk.gray(c.padStart(10))}`);
      }
      i++;
    });

    this.print(output.filter(o => o.length > 0).join("\n"));
  }

  private runForwards(commands: string[]): Set<number>[] {
    const allCommands = this.parseAllCommands(commands);

    const outputZs: Set<number>[] = range(allCommands.length + 1).map(_ => new Set<number>());
    outputZs[0].add(0);

    let commandIndex = 0;

    while (commandIndex < allCommands.length) {
      const currCommands = allCommands[commandIndex];
      for (const inputZ of outputZs[commandIndex].keys()) {
        for (let inputDigit = 1; inputDigit <= 9; inputDigit++) {
          const result = this.runProgram2(inputZ, inputDigit, currCommands);
          outputZs[commandIndex + 1].add(result);
        }
      }

      commandIndex++;
      this.print(`Found ${outputZs[commandIndex].size} for ${commandIndex}`);
    }

    return outputZs;
  }

  private runBackwards(outputZs: Set<number>[], commands: string[], findMax: boolean): string {

    const allCommands = this.parseAllCommands(commands);

    const highestLowestDigits = range(allCommands.length).map(_ => new Map<number, number>());

    // const desired1 = [...outputZs[allCommands.length].values()][0];
    const desired1 = 0;

    let desired = new Map<number, number[][]>();
    desired.set(desired1, [[desired1]]);

    this.print(`Backwards desired ${desired1}`);

    for (let i = 0; i < allCommands.length; i++) {
      const nextDesired = new Map<number, number[][]>();
      const digitIndex = allCommands.length - 1 - i;
      const currCommands = allCommands[digitIndex];

      for (const zInput of outputZs[digitIndex].values()) {
        for (let digitInput = 1; digitInput < 10; digitInput++) {
          const result = this.runProgram2(zInput, digitInput, currCommands);
          if (desired.has(result)) {
            const defaultValue = findMax ? 0 : 9;
            const currHighestLowest = highestLowestDigits[digitIndex].get(result) || defaultValue;
            const highestLowest = findMax
              ? Math.max(currHighestLowest, digitInput)
              : Math.min(currHighestLowest, digitInput);
            highestLowestDigits[digitIndex].set(result, highestLowest);

            const prevZs = desired.get(result)!;
            const newZs: number[][] = [];
            for (const prevZ of prevZs) {
              const newZ = [...prevZ];
              newZ.reverse();
              newZ.push(zInput);
              newZ.reverse();
              newZs.push(newZ);
            }

            if (nextDesired.has(zInput)) {
              nextDesired.get(zInput)!.push(...newZs);
            } else {
              nextDesired.set(zInput, newZs);
            }
          }
        }
      }

      if (nextDesired.size === 0) {
        this.print(`No desired numbers found i=${i}`, 0, "red");
        break;
      }

      this.print(`Found ${nextDesired.size} numbers for digitIndex=${digitIndex}`);
      desired = nextDesired;
    }

    // for (const zs of desired.values()) {
    const zs = desired.get(0)!;
    const allOutputs: number[] = [];
    for (const zss of zs) {
      const output = [];
      for (let i = 1; i < zss.length; i++) {
        const z = zss[i];
        output.push(highestLowestDigits[highestLowestDigits.length - zss.length + i].get(z));
      }
      allOutputs.push(parseInt(output.join(""), 10));
    }

    this.print(`Desired has zero? ${desired.has(0)}`);

    return findMax ? Math.max(...allOutputs).toString() : Math.min(...allOutputs).toString();
  }

  private parseAllCommands(commands: string[]) {
    const allCommands: string[][] = [];
    let currCommands: string[];
    commands.forEach(command => {
      if (command.indexOf("inp") !== -1) {
        currCommands = [];
        allCommands.push(currCommands);
      }
      currCommands.push(command);
    });
    return allCommands;
  }

  private runProgram(state: State, input: number[], commands: string[], print: boolean = false): State {
    let inputIndex = 0;
    for (const command of commands) {
      const splits = command.split(" ");
      const [var1Name, var2] = [...splits.slice(1)];
      const var1Value = state.get(var1Name)!;
      let var2Value: number;
      let inputDigit = null;

      switch (splits[0]) {
        case "inp":
          if (inputIndex >= input.length) {
            this.print("Already used input", 0, "red");
            return state;
          }
          inputDigit = input[inputIndex];
          state.set(var1Name, inputDigit);
          inputIndex++;
          break;
        case "add":
          var2Value = state.getOrParse(var2);
          state.set(var1Name, var1Value + var2Value);
          break;
        case "mul":
          var2Value = state.getOrParse(var2);
          state.set(var1Name, var1Value * var2Value);
          break;
        case "div":
          var2Value = state.getOrParse(var2);
          state.set(var1Name, Math.trunc(var1Value / var2Value));
          break;
        case "mod":
          var2Value = state.getOrParse(var2);
          state.set(var1Name, var1Value % var2Value);
          break;
        case "eql":
          var2Value = state.getOrParse(var2);
          state.set(var1Name, var1Value === var2Value ? 1 : 0);
          break;

        default:
          throw Error(`unknown command ${splits[0]}`);
      }

      if (print) {
        this.print(`${command.padStart(10)} \n            ${state}`,
          0,
          command.indexOf("inp") !== -1 ? "blue" : "black");
      }
    }

    return state;

  }

  private runProgram2(zInput: number, input: number, commands: string[], print: boolean = false): number {
    const a = parseInt(commands[4].split(" ")[2], 10);
    const b = parseInt(commands[5].split(" ")[2], 10);
    const c = parseInt(commands[15].split(" ")[2], 10);

    const z = zInput;
    const w = input;

    // w + c (prev) === w - b (curr)
    if (z % 26 === w - b) {
      return Math.trunc(z / a);
    } else {
      return 26 * Math.trunc(z / a) + w + c;
    }
  }

  part2(): number | string {
    const commands = this.readInput();
    this.findDups(commands);
    const finalInput = [1, 3, 1, 6, 1, 1, 5, 1, 1, 3, 9, 6, 1, 7];
    const r = this.runWithInput(finalInput, commands);
    this.print(`Run with input ${finalInput.join("")} result ${r}`);

    // method 2 - brute foce
    const desiredZs = this.runForwards(commands);
    const desiredDigits = this.runBackwards(desiredZs, commands, false);

    return desiredDigits;
  }
}

(new Day24(undefined, true)).run();
