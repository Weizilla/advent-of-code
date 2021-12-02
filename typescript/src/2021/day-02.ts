import { Solution } from "../solution";

enum ActionType {
  FORWARD = "forward",
  UP = "up",
  DOWN = "down",
}

class Action {
  type: ActionType;
  amount: number;

  constructor(type: ActionType, amount: number) {
    this.type = type;
    this.amount = amount;
  }
}

class Day2 extends Solution {
  constructor(example?: number) {
    super(2, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const actions = inputs.map(i => this.parseInput(i));
    let hor = 0;
    let depth = 0;
    actions.forEach(a => {
      if (a.type === ActionType.FORWARD) {
        hor += a.amount;
      } else if (a.type === ActionType.DOWN) {
        depth += a.amount;
      } else if (a.type === ActionType.UP) {
        depth -= a.amount;
      }
    });

    return hor * depth;
  }

  parseInput(line: string): Action {
    const inputRe = /(\w+) (\d+)/;
    const match = line.match(inputRe)!;
    const actionType = <ActionType>match[1];
    const actionAmount = parseInt(match[2], 10);
    return new Action(actionType, actionAmount);
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const actions = inputs.map(i => this.parseInput(i));
    let hor = 0;
    let depth = 0;
    let aim = 0;
    actions.forEach(a => {
      if (a.type === ActionType.FORWARD) {
        hor += a.amount;
        depth += a.amount * aim;
      } else if (a.type === ActionType.DOWN) {
        aim += a.amount;
      } else if (a.type === ActionType.UP) {
        aim -= a.amount;
      }
    });

    return hor * depth;
  }
}

(new Day2()).run();
