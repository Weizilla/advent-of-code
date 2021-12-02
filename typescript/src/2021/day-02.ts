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
    const [hor, depth] = actions.reduce(this.reducePart1, [0, 0]);
    return hor * depth;
  }

  parseInput(line: string): Action {
    const inputRe = /(\w+) (\d+)/;
    const match = line.match(inputRe)!;
    const actionType = <ActionType>match[1];
    const actionAmount = parseInt(match[2], 10);
    return new Action(actionType, actionAmount);
  }

  reducePart1(prev: [number, number], a: Action): [number, number] {
    let [hor, depth] = prev;
    if (a.type === ActionType.FORWARD) {
      hor += a.amount;
    } else if (a.type === ActionType.DOWN) {
      depth += a.amount;
    } else if (a.type === ActionType.UP) {
      depth -= a.amount;
    }
    return [hor, depth];
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const actions = inputs.map(i => this.parseInput(i));
    const [hor, depth] = actions.reduce(this.reducePart2, [0, 0, 0]);
    return hor * depth;
  }

  reducePart2(prev: [number, number, number], a: Action): [number, number, number] {
    let [hor, depth, aim] = prev;
    if (a.type === ActionType.FORWARD) {
      hor += a.amount;
      depth += a.amount * aim;
    } else if (a.type === ActionType.DOWN) {
      aim += a.amount;
    } else if (a.type === ActionType.UP) {
      aim -= a.amount;
    }
    return [hor, depth, aim];
  }
}

(new Day2()).run();
