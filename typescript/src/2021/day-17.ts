import { Solution } from "../solution";

function rootTriNumber(sum: number): number {
  return (-1 + Math.sqrt(1 + 4 * 2 * sum)) / 2;
}

class Day17 extends Solution {
  constructor(example?: number) {
    super(17, 2021, example);
  }

  part1(): number | string | undefined {
    const input = this.readInput()[0];
    const regex = /target area: x=([\d-]+)..([\d-]+), y=([0-9-]+)..([\d-]+)/;
    const matches = input.match(regex)!;
    const [minX, maxX, minY, maxY] = [...Array.from(matches).slice(1, matches.length)].map(m => parseInt(m, 10));

    const maxVx = maxX; // 1 step
    const minVx = Math.ceil(rootTriNumber(minX)); // max num of steps

    const minVy = minY; // 1 step
    const maxVy = Math.abs(minY); // max num of steps

    this.print(`x=${minVx} - ${maxVx} y=${minVy} - ${maxVy}`);

    let allMaxHeight = minY;

    for (let vX = minVx; vX <= maxVx; vX++) {
      for (let vY = minVy; vY <= maxVy; vY++) {
        const maxHeight = this.runSteps(vX, vY, minX, minY, maxX, maxY, Math.abs(minY * 5));
        if (maxHeight !== null) {
          this.print(`${vX} ${vY} ${maxHeight}`);
          allMaxHeight = Math.max(maxHeight, allMaxHeight);
        }
      }
    }

    this.print(`x ${minX} - ${maxX} y ${minY} ${maxY}`);
    this.print(`max steps ${minVx} ${maxVx}`);
    this.print(`${allMaxHeight}`);

    return allMaxHeight;
  }

  runSteps(vX: number, vY: number, minX: number, minY: number, maxX: number, maxY: number, maxSteps: number): number | null {
    let currX = 0;
    let currY = 0;
    let currVx = vX;
    let currVy = vY;
    let maxHeight = minY;
    for (let s = 0; s <= maxSteps; s++) {
      currX += currVx;
      currY += currVy;
      if (currVx > 0) {
        currVx--;
      } else if (currVx < 0) {
        currVx++;
      }
      currVy--;
      maxHeight = Math.max(maxHeight, currY);
      // this.print(`(${vX},${vY}) step=${s} curr=(${currX},${currY}) max=${maxHeight}`);
      if (minX <= currX && currX <= maxX && minY <= currY && currY <= maxY) {
        return maxHeight;
      }
    }
    return null;
  }

  part2(): number | string | undefined {
    const input = this.readInput()[0];
    const regex = /target area: x=([\d-]+)..([\d-]+), y=([0-9-]+)..([\d-]+)/;
    const matches = input.match(regex)!;
    const [minX, maxX, minY, maxY] = [...Array.from(matches).slice(1, matches.length)].map(m => parseInt(m, 10));

    const maxVx = maxX; // 1 step
    const minVx = Math.ceil(rootTriNumber(minX)); // max num of steps

    const minVy = minY; // 1 step
    const maxVy = Math.abs(minY); // max num of steps

    this.print(`x=${minVx} - ${maxVx} y=${minVy} - ${maxVy}`);

    const allStarts = new Set<string>();

    for (let vX = minVx; vX <= maxVx; vX++) {
      for (let vY = minVy; vY <= maxVy; vY++) {
        const maxHeight = this.runSteps(vX, vY, minX, minY, maxX, maxY, Math.abs(minY * 5));
        if (maxHeight !== null) {
          allStarts.add(`${vX},${vY}`);
        }
      }
    }

    this.print(`x ${minX} - ${maxX} y ${minY} ${maxY}`);
    this.print(`max steps ${minVx} ${maxVx}`);

    return allStarts.size;
  }
}

(new Day17()).run();
