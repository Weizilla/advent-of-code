import { Solution } from "../solution";
import { HashMap } from "../collections";

class Node {
  id: number;
  neighbors: [number, number][];

  constructor(id: number, ...neighbors: [number, number][]) {
    this.id = id;
    this.neighbors = neighbors;
  }

  neighborIds(): number[] {
    return this.neighbors.map(([n, _]) => n).sort((a, b) => a - b);
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

/*
  #############
  #...........#   0 1  2 3  4  5 6
  ###.#.#.#.###      7  9 11 13
    #.#.#.#.#        8 10 12 14
    #########
*/
const Graph = [
  new Node(0, [1, 1]),
  new Node(1, [0, 1], [2, 2], [7, 2]),
  new Node(2, [1, 2], [3, 2], [7, 2], [9, 2]),
  new Node(3, [2, 2], [9, 2], [11, 2], [4, 2]),
  new Node(4, [3, 2], [11, 2], [13, 2], [5, 2]),
  new Node(5, [4, 2], [13, 2], [6, 1]),
  new Node(6, [5, 1]),
  new Node(7, [1, 2], [2, 2], [8, 1]),
  new Node(8, [7, 1]),
  new Node(9, [2, 2], [3, 2], [10, 1]),
  new Node(10, [9, 1]),
  new Node(11, [3, 2], [4, 2], [12, 1]),
  new Node(12, [11, 1]),
  new Node(13, [4, 2], [5, 2], [14, 1]),
  new Node(14, [13, 1]),
];

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

const HallIds = new Set<number>([0, 1, 2, 3, 4, 5, 6]);
const Rooms = new Map<Pod, [number, number]>([
  [Pod.A, [7, 8]],
  [Pod.B, [9, 10]],
  [Pod.C, [11, 12]],
  [Pod.D, [13, 14]],
]);
const OuterIds = new Set<number>([...Rooms.entries()].map(([_p, [n1, _2]]) => n1));
const InnerIds = new Set<number>([...Rooms.entries()].map(([_p, [_1, n2]]) => n2));
const RoomPods = new Map<number, Pod>([...Rooms.entries()].flatMap(([p, [n1, n2]]) => [[n1, p], [n2, p]]));

class State {
  private _pods: Map<Pod, [number, number]>;

  constructor(
    podA1: number,
    podA2: number,
    podB1: number,
    podB2: number,
    podC1: number,
    podC2: number,
    podD1: number,
    podD2: number,
  ) {
    this._pods = new Map<Pod, [number, number]>();
    this._pods.set(Pod.A, [podA1, podA2]);
    this._pods.set(Pod.B, [podB1, podB2]);
    this._pods.set(Pod.C, [podC1, podC2]);
    this._pods.set(Pod.D, [podD1, podD2]);
  }

  clone(): State {
    return new State(
      this.podA[0],
      this.podA[1],
      this.podB[0],
      this.podB[1],
      this.podC[0],
      this.podC[1],
      this.podD[0],
      this.podD[1],
    );
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
    return `a=[${this.podA.sort((a, b) => (a - b))}] b=[${this.podB.sort((a,
      b) => (a - b))}] c=[${this.podC.sort((a, b) => (a - b))}] d=[${this.podD.sort((a, b) => (a - b))}]}`;
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
  private availableNodesCache: Map<string, [number, number][]>;

  constructor(example?: number, forcePrint: boolean = false) {
    super(23, 2021, example, forcePrint);
    this.availableNodesCache = new Map();
  }

  part1(): number | string {
    /*
    #############
    #...........#   0 1  2 3  4  5 6
    ###.#.#.#.###      7  9 11 13
      #.#.#.#.#        8 10 12 14
      #########
    */

    const example = new State(8, 14, 7, 11, 9, 12, 10, 13);
    const real = new State(11, 13, 12, 14, 8, 10, 7, 9);

    const start = this.example === 1 ? example : real;
    this.print(`start ${start.toPrettyString()}`);

    const goal = new State(7, 8, 9, 10, 11, 12, 13, 14);
    this.print(`goal ${goal.toPrettyString()}`, 0, "green");

    const final = this.runPart1(start, goal);
    return final || 0;
  }

  private availableNodes(start: number, state: State): Map<number, number> {
    const key = `${start}|${state.toString()}`;
    const cached = this.availableNodesCache.get(key);
    if (cached) {
      return new Map(cached);
    }
    const next: [number, number][] = [];
    const seen = new Map<number, number>();

    next.push([start, 0]);
    seen.set(start, 0);

    while (next.length > 0) {
      const [curr, currCost] = next.pop()!;
      const currNode = Graph[curr];
      seen.set(curr, currCost);

      const newAvail = currNode.neighbors;
      for (const [newAvailId, newAvailCost] of newAvail) {
        if (state.isEmpty(newAvailId) && !seen.has(newAvailId)) {
          next.push([newAvailId, currCost + newAvailCost]);
          seen.set(newAvailId, currCost + newAvailCost);
        }
      }
    }

    this.availableNodesCache.set(key, [...seen.entries()]);

    return seen;
  }

  private availableNode(nodeId: number, pod: Pod, state: State): [number, number][] {
    /*
    #############
    #...........#   0 1. 2.3 .4 .5 6 hall
    ###.#.#.#.###      7  9 11 13 outer space
      #.#.#.#.#        8 10 12 14 inner space
      #########
    */
    // returns [node id, num of steps]
    const results: [number, number][] = [];
    const allAvailable = this.availableNodes(nodeId, state);
    allAvailable.delete(nodeId);

    // in hallway or in room it doesn't belong in
    if (HallIds.has(nodeId) || (RoomPods.has(nodeId) && pod !== RoomPods.get(nodeId)!)) {
      const [outerSpace, innerSpace] = Rooms.get(pod)!;
      if (allAvailable.get(innerSpace)) {
        // in hall and pod's room is empty, move to inner spot
        results.push([innerSpace, allAvailable.get(innerSpace)!]);
      } else if (state.hasPod(innerSpace) === pod && allAvailable.get(outerSpace)) {
        // in hall and pod's room has same pod, move to outer spot
        results.push([outerSpace, allAvailable.get(outerSpace)!]);
      }
    }
    if (OuterIds.has(nodeId) && (pod !== RoomPods.get(nodeId)! || state.hasPod(nodeId + 1) !== RoomPods.get(nodeId)!)) {
      // in outer room, can move to hallway if it doesn't belong in room or other pod is in room
      [...allAvailable.entries()].filter(([n, _]) => HallIds.has(n)).forEach(([n, c]) => results.push([n, c]));
    }
    if (InnerIds.has(nodeId) && pod !== RoomPods.get(nodeId)!) {
      // in inner room, can move to hallway if it doesn't belong in room
      [...allAvailable.entries()].filter(([n, _]) => HallIds.has(n)).forEach(([n, c]) => results.push([n, c]));
    }

    return results;
  }

  private availableStates(state: State): HashMap<State, number> {
    const results = new HashMap<State, number>(s => s.toString());

    for (const p of Object.values(Pod)) {
      state.getPod(p).forEach(currNode => {
        this.availableNode(currNode, p, state).forEach(([nextNode, multiplier]) => {
          const newState = state.clone();
          newState.setPod(p, currNode, nextNode);
          results.set(newState, multiplier * costs(p));
        });
      });
    }

    return results;
  }

  private runPart1(start: State, goal: State): number | null {
    const allNextStates = new HashMap<State, number>(s => s.toString());
    const allCosts = new HashMap<State, number>(s => s.toString());

    allNextStates.set(start, 0);

    let step = 0;
    while (allNextStates.size() > 0) {
      const sorted = allNextStates.entries().sort(([_1, n1], [_2, n2]) => n2 - n1);
      const [currState, cost] = sorted.pop()!;
      allNextStates.delete(currState);
      // allCosts.set(currState, cost);

      if (cost > 16500) {
        allNextStates.entries().forEach(([s, c], i) => {
          this.print(`${i} cost=${c} ${s.toPrettyString()}`);
        });
        this.print(`Cost too high ${cost}`);
        return null;
      }

      if (step % 10000 === 0) {
        this.print(`step=${step} currCost=${cost} costsSize=${allCosts.size()} nextSize=${allNextStates.size()} availableNodesCacheSize=${this.availableNodesCache.size}`);
        this.print(currState.toPrettyString());
      }

      if (currState.equals(goal)) {
        this.print(`DONE ${allCosts.get(currState)}`);
        return allCosts.get(currState)!;
      }

      const newAvail = this.availableStates(currState).entries();
      for (const [a, c2] of newAvail) {
        if (!allNextStates.has(a) || allNextStates.get(a)! > cost + c2) {
          allNextStates.set(a, cost + c2);
        }
        if (!allCosts.has(a) || allCosts.get(a)! > cost + c2) {
          allCosts.set(a, cost + c2);
        }

        if (a.equals(goal)) {
          this.print(`DONE ${allCosts.get(currState)}`);
          return allCosts.get(currState)!;
        }
      }
      step++;
    }

    allCosts.entries().forEach(([s, c], i) => {
      this.print(`${i} cost=${c} ${s.toPrettyString()}`);
    });
    this.print(`Not found`);
    return null;
  }

  part2(): number | string {
    return 0;
  }
}

(new Day23(undefined, true)).run();

