import { Solution } from "../solution";

class State {
  w: number;
  x: number;
  y: number;
  z: number;
  input: number[];
  inputIndex: number;
  commandIndex: number;
  commandLength: number;

  constructor(input: number[], commandLength: number, w: number = 0, x: number = 0, y: number = 0, z: number = 0, inputIndex = 0, commandIndex = 0) {
    this.w = w;
    this.x = x;
    this.y = y;
    this.z = z;
    this.input = input;
    this.commandLength = commandLength;

    this.inputIndex = inputIndex;
    this.commandIndex = commandIndex;
  }

  clone(): State {
    return new State(this.input, this.commandLength, this.w, this.x, this.y, this.z, this.inputIndex, this.commandIndex);
  }

  key(): string {
    return this.input.slice(0, this.inputIndex).join("");
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

  getInput(): number {
    const r = this.input[this.inputIndex];
    this.inputIndex++;
    return r;
  }

  getAndIncCommandIndex(): number {
    const i = this.commandIndex;
    this.commandIndex++;
    return i;
  }

  hasNext(): boolean {
    return this.commandIndex < this.commandLength;
  }
}

class Day24 extends Solution {
  constructor(example?: number, forcePrint: boolean = false) {
    super(24, 2021, example, forcePrint);
  }

  part1(): number | string {
    const commands = this.readInput();
    const result = this.runPart1(commands);
    return result;
  }

  private runPart1(commands: string[]): number | string {
    const curr = "9".repeat(14).split("").map(n => parseInt(n, 10));
    const end = "1".repeat(14).split("").map(n => parseInt(n, 10));
    // end[end.length - 1] = 1;
    // end[end.length - 2] = 1;


    const cache = new Map<string, State>();

    let i = 0;
    while (!this.equals(curr, end)) {
      // this.print(curr.join(""));

      // const result = this.runProgram(curr, commands, cache);
      // if (result === 0) {
      //   return curr.join("");
      // }

      const result = 0;
      if (i % 100000 === 0) {
        this.print(`i=${i} curr=${curr.join("")} result=${result}`);
      }

      this.decrease(curr);
      i++;

    }

    return 0;
  }

  private equals(n1: number[], n2: number[]): boolean {
    for (let i = 0; i < n1.length; i++) {
      if (n1[i] !== n2[i]) {
        return false;
      }
    }
    return true;
  }

  private decrease(input: number[]) {
    input[input.length - 1]--;

    for (let i = input.length - 1; i >= 0; i--) {
      if (input[i] === 0) {
        input[i] = 9;
        input[i - 1]--;
      }
    }
  }

  private loadState(input: number[], commandLength: number, cache: Map<string, State>): State {
    for (let i = input.length - 1; i >= 0; i--) {
      const key = input.slice(0, i).join("");
      const cached = cache.get(key);
      if (cached) {
        if (key !== cached.key()) {
          throw Error(`Keys do not match ${key} ${cached.key()}`);
        }
        // this.print(`Loaded from cache ${key}`);
        return cached;
      }
    }

    return new State(input, commandLength);
  }

  private runProgram(input: number[], commands: string[], cache: Map<string, State>): number {
    const state = this.loadState(input, commands.length, cache);

    while (state.hasNext()) {
      const splits = commands[state.getAndIncCommandIndex()].split(" ");
      const [var1Name, var2] = [...splits.slice(1)];
      const var1Value = state.get(var1Name)!;
      let var2Value: number;

      switch (splits[0]) {
        case "inp":
          cache.set(state.key(), state.clone());
          state.set(var1Name, state.getInput());
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
    }
    const result = state.get("z")!;

    return result;

  }

  private key(vars: Map<string, number>, i: number, input: number[], inputIndex: number): string {
    let output = `${i}|${input.join("").slice(inputIndex)}|`;
    for (const k of [...vars.keys()].sort()) {
      output += `[${k}:${vars.get(k)!}]`;
    }
    return output;
  }

  part2(): number | string {
    return 0;
  }
}

(new Day24(undefined, true)).run();
