import { print, Solution } from "./solution";
import { HashMap } from "./collections";

class Node {
  id: string;
  weight: number;
  aboveIds: string[];
  aboveNodes: Node[] = [];

  constructor(id: string, weight: number, aboveIds: string[]) {
    this.id = id;
    this.weight = weight;
    this.aboveIds = aboveIds;
  }

  combinedWeight(): number {
    return this.weight + this.aboveNodes.map(n => n.combinedWeight()).reduce((a, b) => a + b, 0);
  }

  sameWeight(): number | null {
    const weights = this.aboveNodes.map(n => n.combinedWeight());
    if (new Set(weights).size === 1) {
      return null;
    }

    const sameAbove = this.aboveNodes.map(n => n.sameWeight()).filter(n => n !== null);

    if (sameAbove.length !== 0) {
      return sameAbove[0];
    }

    const weightCounter = new HashMap<number, number>();
    weights.forEach(w => {
      const count = weightCounter.get(w) || 0;
      weightCounter.set(w, count + 1);
    });

    const desiredWeight = parseInt(Array.from(weightCounter.map.entries())
      .filter(([v, c], i) => c > 1)[0][0], 10);

    const badNode = this.aboveNodes.filter(n => n.combinedWeight() !== desiredWeight)[0];

    return desiredWeight - badNode.combinedWeight() + badNode.weight;
  }
}

class Day7 extends Solution {
  constructor(example?: number) {
    super(7, example);
  }

  part1(): number | string | undefined {
    const programs = this.readPrograms();
    const hasPointer = new Set<string>();
    Array.from(programs.map.values()).flatMap(n => n.aboveIds).forEach(n => hasPointer.add(n));
    const noPointers = Array.from(programs.map.keys()).filter(s => !hasPointer.has(s));

    // this.print(programs);
    print(noPointers);
    return noPointers[0];
  }

  part2(): number | string | undefined {
    const programs = this.readPrograms();
    const hasPointer = new Set<string>();
    Array.from(programs.map.values()).flatMap(n => n.aboveIds).forEach(n => hasPointer.add(n));
    const bottom = Array.from(programs.map.keys()).filter(s => !hasPointer.has(s))[0];

    return programs.get(bottom)?.sameWeight()!;
  }

  readPrograms(): HashMap<string, Node> {
    const programs = new HashMap<string, Node>();
    const inputs = this.readInput();
    const inputRe = /([\w]+) \(([\d]+)\)( -> )*([\w, ]*)/;

    for (const input of inputs) {
      const match = input.match(inputRe)!;
      const id = match[1];
      const weight = parseInt(match[2], 10);
      const aboveIds = match[4] ? match[4].replace(/,/g, "").split(" ") : [];
      const node = new Node(id, weight, aboveIds);
      programs.set(id, node);
    }

    for (const node of programs.map.values()) {
      node.aboveNodes = node.aboveIds.map(n => programs.get(n)!);
    }

    // this.print(programs);
    return programs;
  }
}

(new Day7()).run();
