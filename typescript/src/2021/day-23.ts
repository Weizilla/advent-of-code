import { Solution } from "../solution";
import { HashMap, HashSet } from "../collections";

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

class State {
  private _podA: [number, number];
  private _podB: [number, number];
  private _podC: [number, number];
  private _podD: [number, number];

  constructor(podA1: number, podA2: number, podB1: number, podB2: number, podC1: number, podC2: number, podD1: number, podD2: number) {
    this._podA = [podA1, podA2];
    this._podB = [podB1, podB2];
    this._podC = [podC1, podC2];
    this._podD = [podD1, podD2];
  }

  clone(): State {
    return new State(this.podA[0], this.podA[1], this.podB[0], this.podB[1], this.podC[0], this.podC[1], this.podD[0], this.podD[1]);
  }

  get podA(): [number, number] {
    return this._podA;
  }

  get podB(): [number, number] {
    return this._podB;
  }

  get podC(): [number, number] {
    return this._podC;
  }

  get podD(): [number, number] {
    return this._podD;
  }

  get allLoc(): number[] {
    const all = this.podA.sort((a, b) => (a - b)).concat(
      this.podB.sort((a, b) => (a - b)),
      this.podC.sort((a, b) => (a - b)),
      this.podD.sort((a, b) => (a - b)));
    return all;
  }

  equals(other: State): boolean {
    const otherAllLoc = other.allLoc;
    return this.allLoc.map((v, i) => otherAllLoc[i] === v).filter(a => a).length === this.allLoc.length;
  }

  toString() {
    return `{a=[${this.podA}] b=[${this.podB}] c=[${this.podC}] d=[${this.podD}]}`;
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
    this.print(`start ${start}`);

    const goal = new State(7, 8, 9, 10, 11, 12, 13, 14);
    // const goal = start.clone();
    this.print(`goal ${goal}`, 0, "green");

    // this.print(`goal ${goal}`, 0, "green");

    const cost = this.runPart1(start, goal);

    return cost;
  }

  private availableNode(nodeId: number, pod: string, state: State): [number, number][] {
    // returns [node id, multiplier]
    const results: [number, number][] = [];
    switch (nodeId) {
      case 0:
        results.push([1, 1]);
        break;
      case 1:
        results.push([0, 1], [2, 2], [7, 2]);
        break;
      case 2:
        results.push([1, 2], [3, 2], [7, 2], [9, 2]);
        break;
      case 3:
        results.push([2, 2], [9, 2], [11, 2], [4, 2]);
        break;
      case 4:
        results.push([3, 2], [11, 2], [13, 2], [5, 2]);
        break;
      case 5:
        results.push([4, 2], [13, 2], [6, 1]);
        break;
      case 6:
        results.push([5, 6]);
        break;
      case 7:
        results.push([1, 2], [2, 2], [8, 1]);
        break;
      case 8:
        results.push([7, 1]);
        break;
      case 9:
        results.push([2, 2], [3, 2], [10, 1]);
        break;
      case 10:
        results.push([9, 1]);
        break;
      case 11:
        results.push([3, 2], [4, 2], [12, 1]);
        break;
      case 12:
        results.push([11, 1]);
        break;
      case 13:
        results.push([4, 2], [5, 2], [14, 1]);
        break;
      case 14:
        results.push([13, 1]);
        break;

      //TODO don't let pod go into

      default:
        throw Error(`Unknown id ${nodeId}`);
    }

    const avail = results.filter(([n, _]) => state.allLoc.indexOf(n) === -1);
    return avail;
  }


  private availableStates(state: State): HashMap<State, number> {
    const results = new HashMap<State, number>(s => s.toString());

    state.podA.forEach((currNode, podIndex) => {
      this.availableNode(currNode, "a", state).forEach(([nextNode, multiplier]) => {
        const newState = state.clone();
        newState.podA[podIndex] = nextNode;
        results.set(newState, multiplier);
      });
    });
    state.podB.forEach((currNode, podIndex) => {
      this.availableNode(currNode, "b", state).forEach(([nextNode, multiplier]) => {
        const newState = state.clone();
        newState.podB[podIndex] = nextNode;
        results.set(newState, multiplier * 10);
      });
    });
    state.podC.forEach((currNode, podIndex) => {
      this.availableNode(currNode, "c", state).forEach(([nextNode, multiplier]) => {
        const newState = state.clone();
        newState.podC[podIndex] = nextNode;
        results.set(newState, multiplier * 100);
      });
    });
    state.podD.forEach((currNode, podIndex) => {
      this.availableNode(currNode, "d", state).forEach(([nextNode, multiplier]) => {
        const newState = state.clone();
        newState.podD[podIndex] = nextNode;
        results.set(newState, multiplier * 1000);
      });
    });

    // this.print(results.entries().join("\n"));

    return results;
  }

  private runPart1(start: State, goal: State): number {
    const allNextStates = new HashMap<State, number>(s => s.toString());
    const allCosts = new HashMap<State, number>(s => s.toString());

    allNextStates.set(start, 0);

    let step = 0;
    while (allNextStates.size() > 0) {
      const sorted = allNextStates.entries().sort(([_1, n1], [_2, n2]) => n2 - n1);
      const [currState, cost] = sorted.pop()!;
      allNextStates.delete(currState);
      allCosts.set(currState, cost);

      this.print(`step=${step} currCost=${cost} costs=${allCosts.size()} next=${allNextStates.size()}`);

      if (currState.equals(goal)) {
        this.print(`DONE ${cost}`);
        return cost;
      }

      const newAvail = this.availableStates(currState).entries();
      if (newAvail.length > 0) {
        for (const [a, c2] of newAvail) {

          if (a.equals(goal)) {
            this.print(`DONE ${cost + c2}`);
            return cost + c2;
          }

          if (!allCosts.has(a)) {
            allNextStates.set(a, cost + c2);
          }
          allCosts.set(a, cost + c2);
        }
      }
      step++;
    }

    this.print(`Not found`);
    return 0;
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
