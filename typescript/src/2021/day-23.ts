import { Solution } from "../solution";
import { HashMap } from "../collections";

class Node {
  id: number;
  inner: boolean;
  neighbors: [number, number][];
  _podRoom: Pod | null;

  constructor(id: number, inner: boolean = false, ...neighbors: [number, number][]) {
    this.id = id;
    this.inner = inner;
    this._podRoom = null;
    this.neighbors = neighbors;
  }

  get isHall(): boolean {
    return this._podRoom === null;
  }

  get isRoom(): boolean {
    return this._podRoom !== null;
  }

  get podRoom(): Pod | null {
    return this._podRoom;
  }

  set podRoom(value: Pod | null) {
    this._podRoom = value;
  }

  neighborIds(): number[] {
    return this.neighbors.map(([n, _]) => n).sort((a, b) => a - b);
  }

  toString() {
    return `{Node id=${this.id} hall=${this.isHall} pdoRoom=${this.podRoom?.id || ""} neighbors=${this.neighborIds()
      .join(",")}}`;
  }
}

/*
  #############
  #...........#   0 1  2 3  4  5 6
  ###.#.#.#.###      7  9 11 13
    #.#.#.#.#        8 10 12 14
    #.#.#.#.#       15 17 19 21
    #.#.#.#.#       16 18 20 22
    #########
*/
const Graph = [
  new Node(0, false, [1, 1]),
  new Node(1, false, [0, 1], [2, 2], [7, 2]),
  new Node(2, false, [1, 2], [3, 2], [7, 2], [9, 2]),
  new Node(3, false, [2, 2], [9, 2], [11, 2], [4, 2]),
  new Node(4, false, [3, 2], [11, 2], [13, 2], [5, 2]),
  new Node(5, false, [4, 2], [13, 2], [6, 1]),
  new Node(6, false, [5, 1]),

  new Node(7, false, [1, 2], [2, 2], [8, 1]),
  new Node(8, false, [7, 1], [15, 1]),

  new Node(9, false, [2, 2], [3, 2], [10, 1]),
  new Node(10, false, [9, 1], [17, 1]),

  new Node(11, false, [3, 2], [4, 2], [12, 1]),
  new Node(12, false, [11, 1], [19, 1]),

  new Node(13, false, [4, 2], [5, 2], [14, 1]),
  new Node(14, false, [13, 1], [21, 1]),

  new Node(15, false, [8, 1], [16, 1]),
  new Node(16, true, [15, 1]),

  new Node(17, false, [10, 1], [18, 1]),
  new Node(18, true, [17, 1]),

  new Node(19, false, [12, 1], [20, 1]),
  new Node(20, true, [19, 1]),

  new Node(21, false, [14, 1], [22, 1]),
  new Node(22, true, [21, 1]),
];

class Pod {
  id: string;
  cost: number;
  rooms: Node[];

  constructor(id: string, cost: number, ...roomIds: number[]) {
    this.id = id;
    this.cost = cost;
    this.rooms = roomIds.map(i => Graph[i]);
    for (const n of this.rooms) {
      n.podRoom = this;
    }
  }

  toString(): string {
    return this.id;
  }
}

const Pods = {
  A: new Pod("a", 1, 7, 8, 15, 16),
  B: new Pod("b", 10, 9, 10, 17, 18),
  C: new Pod("c", 100, 11, 12, 19, 20),
  D: new Pod("d", 1000, 13, 14, 21, 22),
};

class State {
  private _pods: HashMap<Pod, number[]>;
  private podNodes: Map<number, Pod>;

  constructor(a: number[], b: number[], c: number[], d: number[]) {
    this._pods = new HashMap<Pod, number[]>(p => p.id);
    this._pods.set(Pods.A, a);
    this._pods.set(Pods.B, b);
    this._pods.set(Pods.C, c);
    this._pods.set(Pods.D, d);
    this.podNodes = new Map<number, Pod>(this._pods.entries().flatMap(([p, ns]) => ns.map(n => [n, p])));
  }

  clone(): State {
    return new State(
      this.getPod(Pods.A),
      this.getPod(Pods.B),
      this.getPod(Pods.C),
      this.getPod(Pods.D),
    );
  }

  getPod(pod: Pod): number[] {
    return this._pods.get(pod)!;
  }

  setPod(pod: Pod, old: number, value: number) {
    const newValues = this.getPod(pod).map(v => (v === old ? value : v));
    this._pods.set(pod, newValues);
    this.podNodes = new Map<number, Pod>(this._pods.entries().flatMap(([p, ns]) => ns.map(n => [n, p])));
  }

  isEmpty(node: number): boolean {
    return this.hasPod(node) === null;
  }

  hasPod(node: number): Pod | null {
    return this.podNodes.get(node) || null;
  }

  get allLoc(): number[] {
    const result: number[] = [];
    Object.values(Pods)
      .forEach(p => this.getPod(p)
        .sort((a, b) => a - b)
        .forEach(n => result.push(n)));
    return result;
  }

  equals(other: State): boolean {
    return this.toString() === other.toString();
  }

  toString() {
    return `{${this.allLoc.join("|")}`;
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
    output += `  |${this.hasPod(15) || "."}`;
    output += `|${this.hasPod(17) || "."}`;
    output += `|${this.hasPod(19) || "."}`;
    output += `|${this.hasPod(21) || "."}|  \n`;
    output += `  |${this.hasPod(16) || "."}`;
    output += `|${this.hasPod(18) || "."}`;
    output += `|${this.hasPod(20) || "."}`;
    output += `|${this.hasPod(22) || "."}|  \n`;
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

    const example = new State([8, 14], [7, 11], [9, 12], [10, 13]);
    const real = new State([11, 13], [12, 14], [8, 10], [7, 9]);

    const start = this.example === 1 ? example : real;
    this.print(`start ${start.toPrettyString()}`);

    const goal = new State([7, 8], [9, 10], [11, 12], [13, 14]);
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
      next.sort(([_1, c1], [_2, c2]) => c2 - c1);
      const [curr] = next.pop()!;
      const currNode = Graph[curr];
      const currCost = seen.get(curr)!;

      const newAvail = currNode.neighbors;
      for (const [newAvailId, newAvailCost] of newAvail) {
        const newCost = currCost + newAvailCost;
        if (state.isEmpty(newAvailId) && (!seen.has(newAvailId) || seen.get(newAvailId)! > currCost)) {
          next.push([newAvailId, newCost]);
          seen.set(newAvailId, newCost);
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
    const node = Graph[nodeId];

    // in hallway or in room it doesn't belong in
    if (node.isHall || (node.isRoom && pod !== node.podRoom)) {
      const spaces = pod.rooms.map(r => r.id);
      const innerSpace = spaces[spaces.length - 1];
      if (allAvailable.get(innerSpace)) {
        // in hall and pod's room is empty, move to inner spot
        results.push([innerSpace, allAvailable.get(innerSpace)!]);
      } else if (pod.rooms.every(n => state.isEmpty(n.id) || state.hasPod(n.id) === pod)) {
        // in hall and pod's room has same pod, move to outer spot
        pod.rooms.filter(n => allAvailable.get(n.id)).forEach(n => results.push([n.id, allAvailable.get(n.id)!]));
      }
    }

    if (node.isRoom && (pod !== node.podRoom || pod.rooms.some(n => state.hasPod(n.id) && state.hasPod(n.id) !== node.podRoom))) {
      // in outer room, can move to hallway if it doesn't belong in room or other pod is in room
      [...allAvailable.entries()].filter(([n, _]) => Graph[n].isHall).forEach(([n, c]) => results.push([n, c]));
    }
    // if (node.inner && pod !== node.podRoom) {
    //   // in inner room, can move to hallway if it doesn't belong in room
    //   [...allAvailable.entries()].filter(([n, _]) => HallIds.has(n)).forEach(([n, c]) => results.push([n, c]));
    // }

    return results;
  }

  private availableStates(state: State): HashMap<State, number> {
    const results = new HashMap<State, number>(s => s.toString());

    for (const p of Object.values(Pods)) {
      state.getPod(p).forEach(currNode => {
        this.availableNode(currNode, p, state).forEach(([nextNode, multiplier]) => {
          const newState = state.clone();
          newState.setPod(p, currNode, nextNode);
          results.set(newState, multiplier * p.cost);
        });
      });
    }

    return results;
  }

  private runPart1(start: State, goal: State): number | null {
    const allNextStates = new HashMap<State, number>(s => s.toString());
    const allCosts = new HashMap<State, number>(s => s.toString());

    allNextStates.set(start, 0);
    allCosts.set(start, 0);

    let step = 0;
    while (allNextStates.size() > 0) {
      const sorted = allNextStates.entries().sort(([_1, n1], [_2, n2]) => n2 - n1);
      const [currState] = sorted.pop()!;
      allNextStates.delete(currState);

      const cost = allCosts.get(currState)!;

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
        const newCost = cost + c2;
        if (!allCosts.has(a) || allCosts.get(a)! > newCost) {
          allCosts.set(a, newCost);
          allNextStates.set(a, newCost);
        }
      }
      step++;
    }

    this.print(`not found`, 0, "red");
    return 0;
  }

  part2(): number | string {
    /*
    #############
    #...........#   0 1  2 3  4  5 6
    ###.#.#.#.###      7  9 11 13
      #.#.#.#.#        8 10 12 14
      #.#.#.#.#       15 17 19 21
      #.#.#.#.#       16 18 20 22
      #########
    */
    const example = new State([16, 19, 14, 22], [7, 17, 11, 12], [9, 10, 20, 21], [8, 15, 18, 13]);

    /*
    ###D#D#A#A###
      #D#C#B#A#
      #D#B#A#C#
      #C#C#B#B#
      #########
     */
    const real = new State([11, 19, 13, 14], [17, 12, 20, 22], [16, 18, 10, 21], [7, 8, 15, 9]);

    const start = this.example === 1 ? example : real;
    this.print(`start ${start.toPrettyString()}`);

    const goal = new State([7, 8, 15, 16], [9, 10, 17, 18], [11, 12, 19, 20], [13, 14, 21, 22]);
    this.print(`goal ${goal.toPrettyString()}`, 0, "green");

    const final = this.runPart1(start, goal);
    return final || 0;
  }
}

(new Day23(undefined, true)).run();
