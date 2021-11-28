import { Solution } from "../solution";

class Day4 extends Solution {
  constructor(example?: number) {
    super(4, 2017, example);
  }

  part1(): number | string | undefined {
    const input = this.readInput();
    const numValid = input.filter(l => this.isValid1(l)).length;
    return numValid;
  }

  isValid1(line: string): boolean {
    const allWords = line.split(" ");
    const uniqueWords = new Set(allWords);
    return allWords.length === uniqueWords.size;
  }

  part2(): number | string | undefined {
    const input = this.readInput();
    const numValid = input.filter(l => this.isValid2(l)).length;
    return numValid;
  }

  isValid2(line: string): boolean {
    const allWords = line.split(" ").map(w => w.split("").sort().join());
    const uniqueWords = new Set(allWords);
    return allWords.length === uniqueWords.size;
  }
}

(new Day4()).run();
