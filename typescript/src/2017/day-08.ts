import { Solution } from "../solution";

class Instruction {
  register: string;
  action: Action;
  actionValue: number;
  condRegister: string;
  condCompare: Compare;
  condValue: number;

  constructor(register: string, action: Action, actionValue: number, condRegister: string, condCompare: Compare, condValue: number) {
    this.register = register;
    this.action = action;
    this.actionValue = actionValue;
    this.condRegister = condRegister;
    this.condCompare = condCompare;
    this.condValue = condValue;
  }
}

enum Action {
  INCREASE = "inc",
  DECREASE = "dec",
}

enum Compare {
  GREATER_THAN = ">",
  LESS_THAN = "<",
  GREATER_EQUAL = ">=",
  LESS_EQUAL = "<=",
  EQUAL = "==",
  NOT_EQUAL = "!=",
}

class Day8 extends Solution {
  constructor(example?: number) {
    super(8, 2017, example);
  }

  part1(): number | string | undefined {
    const instructions = this.readInput().map(n => this.parseInput(n));
    const registers = new Map<string, number>();

    instructions.forEach(i => {
      registers.set(i.register, 0);
      registers.set(i.condRegister, 0);
    });

    instructions.forEach(i => {
      if (this.isCondMet(registers, i.condRegister, i.condCompare, i.condValue)) {
        let value = registers.get(i.register)!;
        switch (i.action) {
          case Action.INCREASE:
            value += i.actionValue;
            break;
          case Action.DECREASE:
            value -= i.actionValue;
            break;
          default:
            throw Error();
        }
        registers.set(i.register, value);
      }
    });

    // print(registers);

    return Math.max(...registers.values());
  }

  isCondMet(registers: Map<string, number>, condReg: string, compare: Compare, condValue: number): boolean {
    switch (compare) {
      case Compare.EQUAL:
        return registers.get(condReg)! === condValue;
      case Compare.GREATER_EQUAL:
        return registers.get(condReg)! >= condValue;
      case Compare.GREATER_THAN:
        return registers.get(condReg)! > condValue;
      case Compare.LESS_EQUAL:
        return registers.get(condReg)! <= condValue;
      case Compare.LESS_THAN:
        return registers.get(condReg)! < condValue;
      case Compare.NOT_EQUAL:
        return registers.get(condReg)! !== condValue;
      default:
        throw Error();
    }
  }

  parseInput(line: string) {
    const lineRe = /(\w+) ([\w]+) ([-\d]+) if (\w+) ([^\s]+) ([-\d]+)/;
    const match = line.match(lineRe)!;
    const register = match[1];
    const action = <Action>match[2];
    const actionValue = parseInt(match[3], 10);
    const condReg = match[4];
    const condCompare = <Compare>match[5];
    const condValue = parseInt(match[6], 10);

    return new Instruction(register, action, actionValue, condReg, condCompare, condValue);
  }

  part2(): number | string | undefined {
    const instructions = this.readInput().map(n => this.parseInput(n));
    const registers = new Map<string, number>();

    let max = 0;

    instructions.forEach(i => {
      registers.set(i.register, 0);
      registers.set(i.condRegister, 0);
    });

    instructions.forEach(i => {
      if (this.isCondMet(registers, i.condRegister, i.condCompare, i.condValue)) {
        let value = registers.get(i.register)!;
        switch (i.action) {
          case Action.INCREASE:
            value += i.actionValue;
            break;
          case Action.DECREASE:
            value -= i.actionValue;
            break;
          default:
            throw Error();
        }
        max = Math.max(max, value);
        registers.set(i.register, value);
      }
    });

    // print(registers);

    return max;
  }
}

(new Day8()).run();
