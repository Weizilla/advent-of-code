import { Solution } from "../solution";

class Node {
  private _parent: PairNode | null;

  constructor() {
    this._parent = null;
  }

  get parent(): PairNode | null {
    return this._parent;
  }

  set parent(value: PairNode | null) {
    this._parent = value;
  }

  isLeft(): boolean {
    return this._parent?.left === this;
  }

  isRight(): boolean {
    return this._parent?.right === this;
  }

  depth(): number {
    return this.parent ? this.parent!.depth() + 1 : 0;
  }

  magnitude(): number {
    return 0;
  }
}

class NumNode extends Node {
  value: number;

  constructor(value: number) {
    super();
    this.value = value;
  }

  magnitude(): number {
    return this.value;
  }

  toString(): string {
    const depthStr = "";
    return `${this.value}${depthStr}`;
  }
}

class PairNode extends Node {
  nodes: Node[];

  constructor() {
    super();
    this.nodes = [];
  }

  addNode(node: Node) {
    this.nodes.push(node);
  }

  get left(): Node | NumNode | null {
    return this.nodes[0];
  }

  set left(value: Node | NumNode | null) {
    if (value) {
      this.nodes[0] = value;
    }
  }

  get right(): Node | NumNode | null {
    return this.nodes[1] || null;
  }

  set right(value: Node | NumNode | null) {
    if (value) {
      this.nodes[1] = value;
    }
  }

  isLeaf(): boolean {
    return this.left instanceof NumNode && this.right instanceof NumNode;
  }

  magnitude(): number {
    return 3 * this.left!.magnitude() + 2 * this.right!.magnitude();
  }

  toString(): string {
    const depthStr = "";
    return `[${this.left?.toString() || ""},${this.right?.toString() || ""}${depthStr}]`;
  }
}

class Day18 extends Solution {
  constructor(example?: number) {
    super(18, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput();
    const nodes = inputs.map(i => this.parseInput(i));
    let curr = nodes[0];

    for (let i = 1; i < nodes.length; i++) {
      const next = nodes[i];
      curr = this.add(curr, next);
      this.print(curr);
      this.reduce(curr);
    }

    this.print(curr);

    return curr.magnitude();
  }

  private reduce(root: PairNode) {
    let didStuff = true;
    while (didStuff) {
      didStuff = false;
      didStuff = this.explode(root) || didStuff;
      if (!didStuff) {
        didStuff = this.split(root) || didStuff;
      }
    }
    this.print(`done ${root}`);
  }

  add(left: PairNode, right: PairNode): PairNode {
    const root = new PairNode();
    root.left = left;
    root.right = right;
    left.parent = root;
    right.parent = root;
    return root;
  }

  findExplode(node: PairNode, leaves: PairNode[]) {
    if (node.isLeaf() && node.depth() >= 4) {
      leaves.push(node);
    } else {
      node.nodes.filter(n => n instanceof PairNode).forEach(n => this.findExplode(<PairNode>n, leaves));
    }
  }

  explode(root: PairNode): boolean {
    let exploded = false;

    const leaves: PairNode[] = [];
    this.findExplode(root, leaves);

    if (!leaves || leaves.length === 0) {
      return false;
    }

    const toExplode = leaves[0];

    this.print(`${root} EXPLODE ${toExplode}`, 1);

    const parentWithLeft = this.findParent(toExplode, n => n.isRight());
    const nextLeft = this.findLeaf(parentWithLeft?.left || null, p => p.right);

    const parentWithRight = this.findParent(toExplode, n => n.isLeft());
    const nextRight = this.findLeaf(parentWithRight?.right || null, p => p.left);

    const zero = new NumNode(0);
    zero.parent = toExplode.parent;
    if (toExplode.isLeft()) {
      toExplode.parent!.left = zero;
    } else if (toExplode.isRight()) {
      toExplode.parent!.right = zero;
    }

    if (nextLeft && toExplode.left instanceof NumNode) {
      nextLeft.value += toExplode.left.value;
      exploded = true;
    }
    if (nextRight && toExplode.right instanceof NumNode) {
      nextRight.value += toExplode.right.value;
      exploded = true;
    }

    return exploded;
  }

  findParent(node: PairNode, fn: (pair: PairNode) => boolean) {
    let curr: PairNode | null = node;
    while (curr && !fn(curr)) {
      curr = curr?.parent || null;
    }
    return curr?.parent || null;
  }

  findLeaf(node: Node | null, fn: (pair: PairNode) => Node | null): NumNode | null {
    if (!node) {
      return null;
    }

    if (node instanceof PairNode) {
      return this.findLeaf(fn(node), fn);
    } else if (node instanceof NumNode) {
      return node;
    }

    throw Error("Unknown");
  }

  split(root: PairNode): boolean {
    const leaves: NumNode[] = [];
    this.findSplit(root, leaves);

    if (!leaves || leaves.length === 0) {
      return false;
    }

    const toSplit = leaves[0];
    this.print(`${root} SPLIT ${toSplit}`, 1);

    const parent = toSplit.parent!;

    const newPair = new PairNode();
    newPair.parent = parent;

    if (toSplit.isLeft()) {
      parent.left = newPair;
    }
    if (toSplit.isRight()) {
      parent.right = newPair;
    }

    const newLeft = new NumNode(Math.floor(toSplit.value / 2));
    newLeft.parent = newPair;
    newPair.left = newLeft;

    const newRight = new NumNode(Math.ceil(toSplit.value / 2));
    newRight.parent = newPair;
    newPair.right = newRight;

    return true;
  }

  findSplit(node: Node, leaves: NumNode[]) {
    if (node instanceof NumNode && node.value >= 10) {
      leaves.push(node);
    } else if (node instanceof PairNode) {
      node.nodes.forEach(n => this.findSplit(n, leaves));
    }
  }

  parseInput(line: string): PairNode {
    let activeNode: PairNode | null = null;
    let head: PairNode | null = null;

    const splits = line.split(/([[\],])/g).filter(i => i);

    for (let i = 0; i < splits.length; i++) {
      const c = splits[i];

      if (c === "[") {
        const n = new PairNode();
        if (activeNode) {
          activeNode.addNode(n);
          n.parent = activeNode;
        } else {
          head = n;
        }
        activeNode = n;
      } else if (c === "]") {
        activeNode = activeNode!.parent;
      } else if (c !== ",") {
        const numNode = new NumNode(parseInt(c, 10));
        numNode.parent = activeNode;
        activeNode!.addNode(numNode);
      }
    }

    return head!;
  }

  part2(): number | string | undefined {
    const inputs = this.readInput();

    let maxSum = 0;

    for (let i = 0; i < inputs.length; i++) {
      for (let j = 0; j < inputs.length; j++) {
        if (i !== j) {
          const mag = this.addAndReduce(this.parseInput(inputs[i]), this.parseInput(inputs[j]));
          maxSum = Math.max(maxSum, mag);
        }
      }
    }

    return maxSum;
  }

  private addAndReduce(left: PairNode, right: PairNode): number {
    const root = this.add(left, right);
    this.reduce(root);
    const mag = root.magnitude();
    this.print(`done ${left} + ${right} = ${root} ${mag}`);
    return mag;
  }
}

(new Day18()).run();
