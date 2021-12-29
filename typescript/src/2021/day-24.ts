import { Solution } from "../solution";
import { range } from "../utils";
import { HashMap, HashSet } from "../collections";

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

  clone(): State {
    return new State(this.w, this.x, this.y, this.z);
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

class Cache {
  private done: HashMap<number[], State>;
  private remaining: HashMap<number, HashSet<string>>;

  constructor() {
    this.done = new HashMap<number[], State>(n => n.join(""));
    this.remaining = new HashMap<number, HashSet<string>>(s => s.toString());
  }

  getDoneCache(curr: number[]): [State | null, number] {
    for (let i = curr.length - 1; i > 1; i--) {
      const newCurr = curr.slice(0, i);
      const cached = this.done.get(newCurr);
      if (cached) {
        return [cached, i];
      }
    }

    return [null, 0];
  }

  inRemainingCache(remaining: number[], state: State): boolean {
    return this.remaining.get(state.z)?.has(remaining.join("")) || false;
  }

  skip(curr: number[], remaining: number[], state: State): number[] {
    const remainings = this.remaining.get(state.z)!.values()
      .filter(r => r.length === remaining.length)
      .map(n => parseInt(n, 10));
    const min = Math.min(...remainings);
    const newRemaining = min.toString().split("").map(n => parseInt(n, 10));

    const newCurr = [...curr];
    for (let i = 0; i < newRemaining.length; i++) {
      newCurr[newCurr.length - i - 1] = newRemaining[newRemaining.length - i - 1];
    }

    // print(`curr=${curr.join("")} oldr=${remaining.join("")} newr=${newRemaining.join("")}
    // newcurr=${newCurr.join("")}`);

    return newCurr;
  }

  add(done: number[], remaining: number[], state: State) {
    const remainings = this.remaining.get(state.z) || new HashSet<string>(s => s.toString());
    this.remaining.set(state.z, remainings);
    remainings.add(remaining.join(""));
    // print(`Add to cached ${remaining.join("")} z: ${state.z}`);

    if (done.length > 0) {
      this.done.set(done, state);
    }
  }

  size(): number {
    return this.remaining.size();
  }

  print(): string {
    let output = "";
    for (const k of this.remaining.keys().sort((a, b) => a - b)) {
      const remainings = [...this.remaining.get(k)!].filter(l => l.length <= 5)
        .map(n => parseInt(n, 10))
        .sort((a, b) => a - b);
      for (const r of remainings) {
        output += ` ${k} ${r}\n`;
      }
    }

    return output;
  }

  printLengths(): string {
    let output = "";
    for (const k of this.remaining.keys().sort((a, b) => a - b)) {
      const lengths = new Map<number, number>();
      const remainings = this.remaining.get(k)!;
      remainings.values().forEach(r => {
        const l = lengths.get(r.length) || 0;
        lengths.set(r.length, l + 1);
      });

      output += `${k}\n`;
      for (const lk of [...lengths.keys()].sort((a, b) => a - b)) {
        output += `  len=${lk} num=${lengths.get(lk)}\n`;
      }
    }

    return output;
  }
}

class Day24 extends Solution {
  constructor(example?: number, forcePrint: boolean = false) {
    super(24, 2021, example, forcePrint);
  }

  part1(): number | string {
    const commands = this.readInput();
    // this.runPart1b(commands);
    // this.findDups(commands);
    // this.runPart1a(commands);
    // const firstHalf = this.runPart1(commands);
    // const secondHalf = this.runBackwards(commands);

    this.runForwards(commands);

    // const diff = [...firstHalf.values()].filter(v => secondHalf.has(v));
    //
    // this.print(`First half size ${firstHalf.size} second half size ${secondHalf.size} diff size ${diff.length}`);

    // this.runWithInput(commands);

    return 1;
  }

  private runWithInput(commands: string[]) {
    const input = "33445566".split("").map(n => parseInt(n, 10));
    const state = this.runProgram(new State(), input, commands);
    this.print(`input=${input.join("")} result=${state}`);
  }

  private findDups(commands: string[]) {
    let i = 0;
    let output: string[][] = range(commands.length).map(_ => []);

    commands.forEach(c => {
      if (c.indexOf("inp") !== -1) {
        i = 0;
      }
      if (output[i].indexOf(`|${c.padStart(10)}`) === -1) {
        output[i].push(`|${c.padStart(10)}`);
      }
      i++;
    });

    this.print(output.filter(o => o.length > 0).join("\n"));
  }

  private runPart1(commands: string[]): Set<number> {
    const allCommands = this.parseAllCommands(commands).slice(0, 7);

    const cached = new Cache();

    let curr = [9, 9, 9, 9, 9, 9, 9];
    const goal = [1, 1, 1, 1, 1, 1, 1];
    let cacheHit = 0;
    let count = 0;
    let state;
    let commandIndex;
    const dedupedResults = new Set<number>();
    while (this.greaterThan(curr, goal)) {
      const [cachedState, newCommandIndex] = cached.getDoneCache(curr);
      if (cachedState) {
        state = cachedState;
        commandIndex = newCommandIndex;
      } else {
        state = new State();
        commandIndex = 0;
      }

      const intermediate: [number[], number[], State][] = [];

      for (let i = commandIndex; i < allCommands.length; i++) {
        const command = allCommands[i];
        const remaining = curr.slice(i);

        if (cached.inRemainingCache(remaining, state)) {
          cacheHit++;
          break;
        } else {
          intermediate.push([curr.slice(0, curr.length - remaining.length), remaining, state.clone()]);
          state = this.runProgram(state, [curr[i]], command);
        }
      }

      intermediate.forEach(([d, r, s]) => cached.add(d, r, s));
      dedupedResults.add(state.z);

      curr = this.decrease(curr);

      if (count % 10000 === 0) {
        this.print(`${curr.join("")} count ${count} cache size ${cached.size()} cache hit ${cacheHit}`, 0, "green");
      }
      count++;
    }

    this.print(`count=${count} deduped=${dedupedResults.size} cache size ${cached.size()} cache hit ${cacheHit}`);

    return dedupedResults;
  }

  private runForwards(commands: string[]) {
    const allCommands = this.parseAllCommands(commands);

    const highestDigits = range(allCommands.length).map(_ => new Map<number, number>());
    const outputZs = new Set<number>();
    let inputZs = [0];
    for (let commandIndex = 0; commandIndex < allCommands.length; commandIndex++) {
      const currCommands = allCommands[commandIndex];
      for (const inputZ of inputZs) {
        for (let inputDigit = 1; inputDigit <= 9; inputDigit++) {
          const result = this.runProgram(new State(0, 0, 0, inputZ), [inputDigit], currCommands);
          outputZs.add(result.z);
          const highestDigit = highestDigits[commandIndex];
          const currHighest = highestDigit.get(result.z) || 0;
          highestDigit.set(result.z, Math.max(currHighest, inputDigit));
        }
      }

      this.print(`Found ${outputZs.size} for command index ${commandIndex}`);
      inputZs = [...outputZs.values()];
      outputZs.clear();
    }
  }

  private runBackwards(commands: string[]) {
    const allCommands = this.parseAllCommands(commands);

    const highestDigits = range(allCommands.length).map(_ => new Map<number, number>());

    let desired = new Map<number, number[][]>();
    desired.set(0, [[0]]);

    for (let i = 0; i < allCommands.length; i++) {
      const nextDesired = new Map<number, number[][]>();
      const digitIndex = allCommands.length - 1 - i;
      const currCommands = allCommands[digitIndex];

      for (let zInput = 0; zInput < 1_000_000; zInput++) {
        for (let digitInput = 1; digitInput < 10; digitInput++) {
          const result = this.runProgram(new State(0, 0, 0, zInput), [digitInput], currCommands);
          if (desired.has(result.z)) {
            const currHighest = highestDigits[digitIndex].get(result.z) || 0;
            const highest = Math.max(currHighest, digitInput);
            highestDigits[digitIndex].set(result.z, highest);

            const prevZs = desired.get(result.z)!;
            const newZs: number[][] = [];
            for (const prevZ of prevZs) {
              const newZ = [...prevZ];
              newZ.reverse();
              newZ.push(zInput);
              newZ.reverse();
              newZs.push(newZ);
            }

            nextDesired.set(zInput, newZs);
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

    for (const zs of desired.values()) {
      for (const zss of zs) {
        let output = "";
        for (let i = 1; i < zss.length; i++) {
          const z = zss[i];
          output += `[${highestDigits[highestDigits.length - zss.length + i].get(z)}]:${z},`;
        }
        this.print(output);
      }
    }

    this.print([...desired.entries()].join("\n"));
    this.print(`Desired has zero? ${desired.has(0)}`);

  }

  private runPart1b(commands: string[]): string {
    const allCommands = this.parseAllCommands(commands);

    const cached = new Cache();

    let curr = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9];
    const goal = [1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9];
    let cacheHit = 0;
    let count = 0;
    let state;
    let commandIndex;
    while (this.greaterThan(curr, goal)) {
      const [cachedState, newCommandIndex] = cached.getDoneCache(curr);
      if (cachedState) {
        state = cachedState;
        commandIndex = newCommandIndex;
      } else {
        state = new State();
        commandIndex = 0;
      }

      const intermediate: [number[], number[], State][] = [];

      let bail = false;
      for (let i = commandIndex; i < allCommands.length; i++) {
        const command = allCommands[i];
        const remaining = curr.slice(i);

        // if (remaining.length <= 2) {
        // this.print(`Remaining curr=${curr.join("")} remaining=${remaining.join("")} state=${state.toString()}`);
        // }

        if (cached.inRemainingCache(remaining, state)) {
          // this.print(`Found cached  curr=${curr.join("")} remaining=${remaining.join("")}
          // state=${state.toString()}`);
          curr = cached.skip(curr, remaining, state);
          bail = true;
          cacheHit++;
          break;
        } else {
          intermediate.push([curr.slice(0, curr.length - remaining.length), remaining, state.clone()]);
          // this.print(`${remaining} ${state}`, 2, "blue");
          state = this.runProgram(state, [curr[i]], command);
        }
      }

      if (bail || state.z !== 0) {
        intermediate.forEach(([d, r, s]) => cached.add(d, r, s));
      } else {
        this.print(`DONE ${curr.join}`);
        return curr.join("");
      }

      // this.print(`${curr.join("")} ${state}`);
      // this.print(intermediate);
      curr = this.decrease(curr);

      if (count % 10000 === 0) {
        this.print(`${curr.join("")} count ${count} cache size ${cached.size()} cache hit ${cacheHit}`, 0, "green");
      }
      count++;
    }

    // this.print(`Failed cache ${failed.print()}`);
    this.print(`NOT FOUND checked ${count} cache size ${cached.size()} cache hit ${cacheHit}`, 0, "red");

    return "";
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

  private greaterThan(n1: number[], n2: number[]): boolean {
    return parseInt(n1.join(""), 10) > parseInt(n2.join(""), 10);
  }

  private decrease(input: number[], amount: number = 1): number[] {
    const decreased = parseInt(input.join(""), 10) - amount;
    const asArray = decreased.toString().split("").map(n => parseInt(n, 10));

    for (let i = asArray.length - 1; i >= 0; i--) {
      if (asArray[i] === 0) {
        asArray[i] = 9;
        asArray[i - 1]--;
      }
    }

    return asArray;
  }

  // private loadState(input: number[], commandLength: number, cache: Map<string, State>): State {
  //   for (let i = input.length - 1; i >= 0; i--) {
  //     const key = input.slice(0, i).join("");
  //     const cached = cache.get(key);
  //     if (cached) {
  //       if (key !== cached.key()) {
  //         throw Error(`Keys do not match ${key} ${cached.key()}`);
  //       }
  //       // this.print(`Loaded from cache ${key}`);
  //       return cached;
  //     }
  //   }
  //
  //   return new State(input, commandLength);
  // }

  private runProgram(state: State, input: number[], commands: string[]): State {
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

      // this.print(`${command.padStart(10)} \n            ${state}`, 0, command.indexOf("inp") !== -1 ? "blue" :
      // "black");
    }

    return state;

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
