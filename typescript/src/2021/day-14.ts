import { Solution } from "../solution";
import { print } from "../utils";

class Node {
  value: string;
  next: Node | null;

  constructor(value: string) {
    this.value = value;
    this.next = null;
  }

  toString(): string {
    return `${this.value} ${this.next?.value || ""}`;
  }
}

class Day14 extends Solution {
  constructor(example?: number) {
    super(14, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const rules = this.readRules(inputs);

    let polymer = inputs[0];
    for (let i = 0; i < 10; i++) {
      polymer = this.runStep(polymer, rules);
    }
    return this.calcMinMaxDiff(polymer);
  }

  private readRules(inputs: string[]) {
    const rules = new Map<string, string>();

    for (let i = 1; i < inputs.length; i++) {
      const [input, _, output] = [...inputs[i].split(" ")];
      rules.set(input, output);
    }
    return rules;
  }

  private calcMinMaxDiff(polymer: string) {
    const counts = new Map<string, number>();
    polymer.split("").forEach(p => {
      const count = counts.get(p) || 0;
      counts.set(p, count + 1);
    });

    const maxNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n2 - n1)[0][1];
    const minNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n1 - n2)[0][1];

    this.print(`${minNum} ${maxNum}`);

    return maxNum - minNum;
  }

  runStep(input: string, rules: Map<string, string>): string {
    let output = "";

    for (let i = 0; i < input.length; i++) {
      if ((i + 1) < input.length) {
        const pair = `${input[i]}${input[i + 1]}`;
        if (rules.has(pair)) {
          const added = `${input[i]}${rules.get(pair)!}`;
          output += added;
          // this.print(`${pair} ${added} ${output}`);
        } else {
          output += input[i];
          // this.print(`${input[i]} ${input[i]} ${output}`);
        }
      } else {
        output += input[i];
        // this.print(`${input[i]} ${input[i]} ${output}`);
      }
    }

    return output;
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();
    const rules = this.readRules(inputs);

    const polymer = inputs[0].split("");
    const head = new Node(polymer[0]);
    let prev = head;
    for (let i = 1; i < polymer.length; i++) {
      const n = new Node(polymer[i]);
      prev.next = n;
      prev = n;
    }

    this.printList(head);

    for (let i = 0; i < 40; i++) {
      const numAdded = this.runStep2(head, rules);
      print(`${i} Num added ${numAdded}`);
    }

    this.printList(head);

    return head.value;
  }

  printList(head: Node) {
    let output = "";
    let curr: Node | null = head;
    while (curr !== null) {
      output += curr.value;
      curr = curr.next;
    }
    this.print(output);
  }

  runStep2(head: Node | null, rules: Map<string, string>): number {
    let numAdded = 0;
    let curr: Node | null = head;
    while (curr !== null && curr.next) {
      const pair = `${curr.value}${curr.next.value}`;
      const insert = rules.get(pair);
      if (insert) {
        const i = new Node(insert);
        const next = curr.next;
        curr.next = i;
        i.next = next;
        curr = next;
        numAdded++;
      } else {
        curr = curr.next;
      }
    }
    return numAdded;
  }

  private calcMinMaxDiff2(head: Node) {
    const counts = new Map<string, number>();

    let curr: Node | null = head;
    while (curr !== null) {
      const count = counts.get(curr.value) || 0;
      counts.set(curr.value, count + 1);
      curr = curr.next;
    }

    const maxNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n2 - n1)[0][1];
    const minNum = Array.from([...counts.entries()]).sort(([_1, n1], [_2, n2]) => n1 - n2)[0][1];

    this.print(`${minNum} ${maxNum}`);

    return maxNum - minNum;
  }
}

(new Day14()).run();
