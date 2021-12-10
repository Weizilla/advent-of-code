import { Solution } from "../solution";
import { sum } from "../utils";

const opens = ["(", "[", "{", "<"];
const closes = [")", "]", "}", ">"];

function isOpen(char: string): boolean {
  return opens.indexOf(char) > -1;
}

function isClose(char: string): boolean {
  return closes.indexOf(char) > -1;
}

function opp(char: string): string {
  if (isOpen(char)) {
    return closes[opens.indexOf(char)];
  } else {
    return opens[closes.indexOf(char)];
  }
}


enum Status {
  OK,
  INCOMPLETE,
  CORRUPT,
}

class Day10 extends Solution {
  constructor(example?: number) {
    super(10, 2021, example);
  }

  part1(): number | string | undefined {
    const lines = this.readInput();
    const p = lines
      .map(this.checkLine)
      .filter(([s, _]) => s === Status.CORRUPT)
      .map(([_, c]) => this.points(c));
    return sum(p);
  }

  points(char: string | null): number {
    const p: { [key: string]: number } = {
      ")": 3, "]": 57, "}": 1197, ">": 25137,
    };
    return p[char!]!;
  }

  checkLine(line: string): [Status, string | null] {
    const chars = line.split("");
    const stack: string[] = [];
    while (chars.length > 0) {
      const next = chars.reverse().pop()!;
      chars.reverse();

      // print(`Check next ${next} stack ${stack.join("")} chars ${chars.join("")}`);
      if (isOpen(next)) {
        stack.push(next);
      } else {
        if (stack.length === 0) {
          // print(`Invalid next ${next} stack ${stack.join("")} chars ${chars.join("")}`);
          return [Status.CORRUPT, next];
        }

        const prev = stack.pop();
        if (prev !== opp(next)) {
          // print(`Invalid next ${next} stack ${stack.join("")} chars ${chars.join("")}`);
          return [Status.CORRUPT, next];
        }
      }
    }

    if (stack.length > 0) {
      return [Status.INCOMPLETE, stack.join("")];
    }

    return [Status.OK, null];
  }

  part2(): number | string | undefined {
    const lines = this.readInput();
    const points = lines
      .map(this.checkLine)
      .filter(([s, _]) => s === Status.INCOMPLETE)
      .map(([_, c]) => this.points2(c));
    points.sort((a, b) => a - b);
    return points[Math.trunc(points.length / 2)];
  }

  points2(stack: string | null): number {
    const pointStack = stack!.split("").reverse();
    let points = 0;
    pointStack.forEach(c => {
      points *= 5;
      points += opens.indexOf(c) + 1;
    });
    return points;
  }
}

(new Day10()).run();
