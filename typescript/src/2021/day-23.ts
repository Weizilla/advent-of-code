import { Solution } from "../solution";
import { HashMap, HashSet } from "../collections";
import { sum } from "../utils";

class Node {
  id: number;
  neighbors: Node[];

  constructor(id: number) {
    this.id = id;
    this.neighbors = [];
  }

  neighborIds(): number[] {
    return this.neighbors.map(n => n.id).sort((a, b) => a - b);
  }

  toString() {
    return `{Node id=${this.id} neighbors=${this.neighborIds().join(",")}}`;
  }
}

enum Pod {
  A = "a",
  B = "b",
  C = "c",
  D = "d",

}

function costs(pod: Pod): number {
  switch (pod) {
    case Pod.A:
      return 1;
    case Pod.B:
      return 10;
    case Pod.C:
      return 100;
    case Pod.D:
      return 1000;
    default:
      throw Error();
  }
}

class State {
  private _pods: Map<Pod, [number, number]>;
  cost: number = 0;

  constructor(podA1: number, podA2: number, podB1: number, podB2: number, podC1: number, podC2: number, podD1: number, podD2: number, cost: number = 0) {
    this._pods = new Map<Pod, [number, number]>();
    this._pods.set(Pod.A, [podA1, podA2]);
    this._pods.set(Pod.B, [podB1, podB2]);
    this._pods.set(Pod.C, [podC1, podC2]);
    this._pods.set(Pod.D, [podD1, podD2]);
    this.cost = cost;
  }

  clone(): State {
    return new State(this.podA[0], this.podA[1], this.podB[0], this.podB[1], this.podC[0], this.podC[1], this.podD[0], this.podD[1], this.cost);
  }

  getPod(pod: Pod): [number, number] {
    return this._pods.get(pod)!;
  }

  setPod(pod: Pod, old: number, value: number) {
    const [c1, c2] = this.getPod(pod);
    if (c1 === old) {
      this._pods.set(pod, [value, c2]);
    } else if (c2 === old) {
      this._pods.set(pod, [c1, value]);
    } else {
      throw new Error("old not found");
    }
  }

  get podA(): [number, number] {
    return this.getPod(Pod.A);
  }

  get podB(): [number, number] {
    return this.getPod(Pod.B);
  }

  get podC(): [number, number] {
    return this.getPod(Pod.C);
  }

  get podD(): [number, number] {
    return this.getPod(Pod.D);
  }

  isEmpty(node: number): boolean {
    return this.allLoc.indexOf(node) === -1;
  }

  allEmpty(...nodes: number[]): boolean {
    return nodes.every(n => this.isEmpty(n));
  }

  isEnterable(allowedPod: Pod, numA: number, numB: number): boolean {
    if (this.allEmpty(numA, numB)) {
      return true;
    }

    if (this.isEmpty(numA) && this.hasPod(numB) === allowedPod) {
      return true;
    }

    if (this.isEmpty(numB) && this.hasPod(numA) === allowedPod) {
      return true;
    }

    return false;
  }

  hasPod(node: number): Pod | null {
    for (const p of Object.values(Pod)) {
      if (this._pods.get(p)!.some(n => n === node)) {
        return p;
      }
    }
    return null;
  }

  get allLoc(): number[] {
    const all = this.podA.sort((a, b) => (a - b)).concat(
      this.podB.sort((a, b) => (a - b)),
      this.podC.sort((a, b) => (a - b)),
      this.podD.sort((a, b) => (a - b)),
    );
    return all;
  }

  equals(other: State): boolean {
    const otherAllLoc = other.allLoc;
    return this.allLoc.map((v, i) => otherAllLoc[i] === v).filter(a => a).length === this.allLoc.length;
  }

  toString() {
    return `{cost=${this.cost} a=[${this.podA.sort((a, b) => (a - b))}] b=[${this.podB.sort((a, b) => (a - b))}] c=[${this.podC.sort((a, b) => (a - b))}] d=[${this.podD.sort((a, b) => (a - b))}]}`;
  }

  toPrettyString() {
    let output = `\n-------------\n|`;
    output += this.hasPod(0) || ".";
    output += this.hasPod(1) || ".";
    output += " ";
    output += this.hasPod(2) || ".";
    output += " ";
    output += this.hasPod(3) || ".";
    output += " ";
    output += this.hasPod(4) || ".";
    output += " ";
    output += this.hasPod(5) || ".";
    output += this.hasPod(6) || ".";
    output += "|\n";
    output += `--|${this.hasPod(7) || "."}`;
    output += `|${this.hasPod(9) || "."}`;
    output += `|${this.hasPod(11) || "."}`;
    output += `|${this.hasPod(13) || "."}|--\n`;
    output += `  |${this.hasPod(8) || "."}`;
    output += `|${this.hasPod(10) || "."}`;
    output += `|${this.hasPod(12) || "."}`;
    output += `|${this.hasPod(14) || "."}|  \n`;
    output += `  ---------`;

    return output.toUpperCase();
  }
}

class Day23 extends Solution {
  constructor(example?: number, forcePrint: boolean = false) {
    super(23, 2021, example, forcePrint);
  }

  part1(): number | string {
    /*
    #############
    #...........#   0 1  2 3  4  5 6
    ###.#.#.#.###      7  9 11 13
      #.#.#.#.#        8 10 12 14
      #########
    */

    /*
    #############
    #...........#
    ###B#C#B#D###
      #A#D#C#A#
      #########

      a = 8, 14
      b = 7, 11
      c = 9, 12
      d = 10, 13
    */

    const start = new State(8, 14, 7, 11, 9, 12, 10, 13);
    this.print(`start ${start.toPrettyString()}`);

    const goal = new State(7, 8, 9, 10, 11, 12, 13, 14);
    // const goal = start.clone();
    // goal.podB[0] = 6;
    // goal.podC[0] = 0;
    this.print(`goal ${goal.toPrettyString()}`, 0, "green");

    // this.print(`goal ${goal}`, 0, "green");

    const final = this.runPart1(start, goal);

    return final.cost;
  }

  private availableNode(nodeId: number, pod: Pod, state: State): [number, number][] {
    /*
    #############
    #...........#   0 1. 2.3 .4 .5 6
    ###.#.#.#.###      7  9 11 13
      #.#.#.#.#        8 10 12 14
      #########
    */
    // returns [node id, multiplier]
    const results: [number, number][] = [];
    switch (nodeId) {
      case 0:
        if (state.isEmpty(1)) {
          if (pod === Pod.A && state.isEmpty(7) && state.isEmpty(8)) {
            results.push([8, 4]);
          } else if (pod === Pod.A && state.isEmpty(7) && state.hasPod(8) === pod) {
            results.push([7, 3]);
          }
        }
        break;
      case 1:
        results.push([0, 1], [2, 2]);
        if (pod === Pod.A && state.isEnterable(pod, 7, 8)) {
          results.push([7, 2]);
        }
        break;
      case 2:
        results.push([1, 2], [3, 2]);
        if (pod === Pod.A && state.isEnterable(pod, 7, 8)) {
          results.push([7, 2]);
        }
        if (pod === Pod.B && state.isEnterable(pod, 9, 10)) {
          results.push([9, 2]);
        }
        break;
      case 3:
        results.push([2, 2], [4, 2]);
        if (pod === Pod.B && state.isEnterable(pod, 9, 10)) {
          results.push([9, 2]);
        }
        if (pod === Pod.C && state.isEnterable(pod, 11, 12)) {
          results.push([11, 2]);
        }
        break;
      case 4:
        results.push([3, 2], [5, 2]);
        if (pod === Pod.C && state.isEnterable(pod, 11, 12)) {
          results.push([11, 2]);
        }
        if (pod === Pod.D && state.isEnterable(pod, 13, 14)) {
          results.push([13, 2]);
        }
        break;
      case 5:
        results.push([4, 2], [6, 1]);
        if (pod === Pod.D && state.isEnterable(pod, 13, 14)) {
          results.push([13, 2]);
        }
        break;
      case 6:
        results.push([5, 6]);
        break;
      case 7:
        results.push([8, 1]);
        if (pod !== Pod.A) {
          results.push([1, 2], [2, 2]);
        }
        break;
      case 8:
        if (pod !== Pod.A) {
          results.push([7, 1]);
        }
        break;
      case 9:
        results.push([10, 1]);
        if (pod !== Pod.B) {
          results.push([2, 2], [3, 2]);
        }
        break;
      case 10:
        if (pod !== Pod.B) {
          results.push([9, 1]);
        }
        break;
      case 11:
        results.push([12, 1]);
        if (pod !== Pod.C) {
          results.push([3, 2], [4, 2]);
        }
        break;
      case 12:
        if (pod !== Pod.C) {
          results.push([11, 1]);
        }
        break;
      case 13:
        results.push([14, 1]);
        if (pod !== Pod.D) {
          results.push([4, 2], [5, 2], [14, 1]);
        }
        break;
      case 14:
        if (pod !== Pod.D) {
          results.push([13, 1]);
        }
        break;
      default:
        throw Error(`Unknown id ${nodeId}`);
    }

    const avail = results.filter(([n, _]) => state.allLoc.indexOf(n) === -1);
    return avail;
  }

  private distance(pod: Pod, curr: number): number {
    /*
    #############
    #...........#   0 1  2 3  4  5 6
    ###.#.#.#.###      7  9 11 13
      #.#.#.#.#        8 10 12 14
      #########
    */

    const dist = new Map<Pod, number[]>();
    dist.set(Pod.A, [4, 3, 3, 4, 6, 8, 9, 1, 0, 5, 6, 7, 8, 9, 10]);
    dist.set(Pod.B, [6, 5, 3, 3, 5, 7, 8, 5, 6, 1, 0, 5, 6, 7, 8]);
    dist.set(Pod.C, [8, 7, 5, 3, 3, 5, 6, 7, 8, 5, 6, 1, 0, 5, 6]);
    dist.set(Pod.D, [9, 8, 6, 4, 3, 3, 4, 9, 10, 7, 8, 5, 6, 1, 0]);

    const d = dist.get(pod)![curr];
    return d;
  }

  private stateDistance(state: State): number {
    let dist = 0;
    for (const p of Object.values(Pod)) {
      dist += sum(state.getPod(p).map(n => this.distance(p, n)));
    }
    return dist;
  }

  private availableStates(state: State): HashMap<State, number> {
    const results = new HashMap<State, number>(s => s.toString());

    for (const p of Object.values(Pod)) {
      state.getPod(p).forEach(currNode => {
        this.availableNode(currNode, p, state).forEach(([nextNode, multiplier]) => {
          const newState = state.clone();
          newState.setPod(p, currNode, nextNode);
          newState.cost += multiplier * costs(p);
          results.set(newState, multiplier * costs(p));
        });
      });
    }
    // this.print(results.entries().join("\n"));

    return results;
  }

  private runPart1(start: State, goal: State): State {
    const allNextStates = new HashMap<State, number>(s => s.toString());
    const allCosts = new HashMap<State, number>(s => s.toString());

    allNextStates.set(start, 0);

    let step = 0;
    while (allNextStates.size() > 0) {
      const sorted = allNextStates.entries().sort(([_1, n1], [_2, n2]) => n2 - n1);
      const [currState, cost] = sorted.pop()!;
      allNextStates.delete(currState);
      // allCosts.set(currState, cost);

      if (step % 1000 === 0) {
        this.print(`step=${step} currCost=${cost} costs=${allCosts.size()} next=${allNextStates.size()}`);
        this.print(currState.toPrettyString());
      }

      if (currState.equals(goal)) {
        this.print(`DONE ${cost}`);
        return currState;
      }

      const newAvail = this.availableStates(currState).entries();
      for (const [a, c2] of newAvail) {
        if (a.equals(goal)) {
          this.print(`DONE ${cost + c2}`);
          return a;
        }

        if (!allCosts.has(a)) {
          allNextStates.set(a, cost + c2);
          allCosts.set(a, cost + c2);
        } else if (allCosts.get(a)! > cost + c2) {
          allCosts.set(a, cost + c2);
        }
      }
      step++;
    }

    this.print(`Not found`);
    return new State(0, 0, 0, 0, 0, 0, 0, 0);
  }

  private example1() {
    //     n1
    // n0       n3
    //     n2
    const n0 = new Node(0);
    const n1 = new Node(1);
    const n2 = new Node(2);
    const n3 = new Node(3);
    const nodes = new Map<number, Node>();
    [n0, n1, n2, n3].forEach(n => nodes.set(n.id, n));

    n0.neighbors.push(n1, n2);
    n1.neighbors.push(n0, n3);
    n2.neighbors.push(n0, n3);
    n3.neighbors.push(n1, n2);

    const pods = new Map<string, number>();
    pods.set("a", 0);
    pods.set("b", 3);

    this.print([...nodes.values()].join("\n"));
    this.print([...pods.entries()].join("\n"));

    // this.print(this.allAvailable(n0, nodes, pods, new Set<number>()));

    const goal = new Map<string, number>();
    goal.set("a", 3);
    goal.set("b", 0);

    const seen = new HashSet<Map<string, number>>(Day23.mapToStr);
    seen.add(pods);

    const steps = this.step(pods, nodes, goal, []).sort((a, b) => a.length - b.length)[0];
    this.print(steps.map(s => Day23.mapToStr(s)).join(""));
  }

  private static mapToStr(m: Map<string, number>): string {
    let output = "{";
    const keys = [...m.keys()].sort();
    for (const key of keys) {
      output += `[${key}:${m.get(key)!}]`;
    }
    output += "}";
    return output;
  }

  step(pods: Map<string, number>, nodes: Map<number, Node>, goal: Map<string, number>, steps: Map<string, number>[]): Map<string, number>[][] {
    if (this.isSame(pods, goal)) {
      this.print("Done");
      return [steps];
    }

    const allSteps = [];

    for (const [pod, loc] of pods) {
      const pNode = nodes.get(loc)!;
      const pAvailNodes = this.allAvailable(pNode, nodes, pods, new Set<number>());

      for (const pa of pAvailNodes) {
        const newPods = new Map<string, number>([...pods.entries()]);
        newPods.set(pod, pa.id);

        const seen = new HashSet<Map<string, number>>(Day23.mapToStr, steps);

        if (!seen.has(newPods)) {
          seen.add(newPods);

          this.print(`avail ${pa.id}`);
          this.print([...newPods.entries()].join("\n"));

          const newSteps = [...steps];
          newSteps.push(newPods);

          const childSteps = this.step(newPods, nodes, goal, newSteps);
          allSteps.push(...childSteps);
        }
      }
    }

    return allSteps;

  }

  isSame(a: Map<string, number>, b: Map<string, number>): boolean {
    for (const [k, v] of a) {
      if (b.get(k) !== v) {
        return false;
      }
    }

    return true;
  }


  allAvailable(start: Node, nodes: Map<number, Node>, pods: Map<string, number>, seen: Set<number>): Node[] {
    const avail = start.neighborIds().filter(n => !seen.has(n));
    avail.forEach(a => seen.add(a));
    const availNodes = avail.map(a => nodes.get(a)!).filter(n => [...pods.values()].indexOf(n.id) === -1);
    availNodes.push(...availNodes.flatMap(a => this.allAvailable(a, nodes, pods, seen)));
    return availNodes;
  }

  part2(): number | string {
    return 0;
  }
}

(new Day23(1)).run();
