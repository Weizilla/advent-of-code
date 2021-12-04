import { Solution } from "../solution";
import { sum } from "../utils";

class RowCol {
  boardId: number;
  nums: Set<number>;
  isRow: boolean;

  constructor(boardId: number, nums: Iterable<number>, isRow: boolean) {
    this.boardId = boardId;
    this.nums = new Set<number>(nums);
    this.isRow = isRow;
  }

  markNum(num: number) {
    this.nums.delete(num);
  }

  isWinner(): boolean {
    return this.nums.size === 0;
  }

  toString(): string {
    return `${this.boardId} ${JSON.stringify([...this.nums])}`;
  }

  toJSON(): string {
    return this.toString();
  }
}

class Boards {
  numToRowCol: Map<number, RowCol[]>;
  boardToRowCol: Map<number, RowCol[]>;
  numBoards: number;

  constructor(inputs: string[]) {
    this.numToRowCol = new Map<number, RowCol[]>();
    this.boardToRowCol = new Map<number, RowCol[]>();
    this.numBoards = (inputs.length - 1) / 5;

    for (let i = 0; i < this.numBoards; i++) {
      const allRowCol = [];
      const cols: number[][] = new Array(5).fill(0).map(_ => []);
      for (let j = 0; j < 5; j++) {
        const rowInts = inputs[1 + (i * 5) + j].trim().split(/\s+/).map(n => parseInt(n, 10));
        const rowCol = new RowCol(i, rowInts, true);
        allRowCol.push(rowCol);

        for (let k = 0; k < 5; k++) {
          cols[k].push(rowInts[k]);
        }
      }

      for (const c of cols) {
        allRowCol.push(new RowCol(i, c, false));
      }

      for (const rowCol of allRowCol) {
        for (const v of rowCol.nums) {
          if (!this.numToRowCol.get(v)) {
            this.numToRowCol.set(v, []);
          }
          this.numToRowCol.get(v)!.push(rowCol);
        }
      }

      this.boardToRowCol.set(i, allRowCol);
    }
  }
}

class Day4 extends Solution {
  constructor(example?: number) {
    super(4, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const bingoNums = inputs[0].split(",").map(n => parseInt(n, 10));

    const board = new Boards(inputs);

    for (const b of bingoNums) {
      for (const r of board.numToRowCol.get(b)!) {
        r.markNum(b);
        if (r.isWinner()) {
          const s = sum(board.boardToRowCol.get(r.boardId)!.filter(rc => rc.isRow).map(rc => sum(rc.nums)));
          return s * b;
        }
      }
    }

    return 0;
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const bingoNums = inputs[0].split(",").map(n => parseInt(n, 10));

    const board = new Boards(inputs);
    const winners = new Set<number>();

    for (const b of bingoNums) {
      for (const r of board.numToRowCol.get(b)!) {
        r.markNum(b);
        if (r.isWinner()) {
          winners.add(r.boardId);
          const s = sum(board.boardToRowCol.get(r.boardId)!.filter(rc => rc.isRow).map(rc => sum(rc.nums)));
          if (winners.size === board.numBoards) {
            return s * b;
          }
        }
      }
    }

    return 0;
  }
}

(new Day4()).run();
