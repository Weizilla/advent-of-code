interface HashKey {
  toString(): string;
}

class HashSet<K extends HashKey> {
  private _values: Map<string, K>;

  constructor() {
    this._values = new Map<string, K>();
  }

  has(value: K): boolean {
    return this._values.has(value.toString());
  }

  add(value: K) {
    this._values.set(value.toString(), value);
  }

  values(): Array<K> {
    return Array.from(this._values.values());
  }

  delete(value: K) {
    this._values.delete(value.toString());
  }

  size() {
    return this._values.size;
  }

  toString(): string {
    return JSON.stringify(Array.from(this._values.keys()));
  }
}

class HashMap<K extends HashKey, V> {
  private _map: Map<string, V>;
  private _keys: Map<string, K>;

  constructor() {
    this._map = new Map<string, V>();
    this._keys = new Map<string, K>();
  }

  entries(): Array<[K, V]> {
    return Array.from(this._map.entries(), ([k, v]) => [this._keys.get(k)!, v]);
  }

  keys(): Array<K> {
    return Array.from(this._keys.values());
  }

  values(): Array<V> {
    return Array.from(this._map.values());
  }

  get(key: K): V | undefined {
    return this._map.get(key.toString());
  }

  set(key: K, value: V) {
    this._map.set(key.toString(), value);
    this._keys.set(key.toString(), key);
  }

  size() {
    return this._map.size;
  }

  toString(): string {
    return JSON.stringify(Array.from(this.entries()));
  }
}

class Counter {
  private _map: Map<string, number>;

  constructor() {
    this._map = new Map<string, number>();
  }

  count(value: string) {
    this.add(value, 1);
  }

  add(value: string, inc: number) {
    const count = this._map.get(value) || 0;
    this._map.set(value, count + inc);
  }

  maxCount(): number {
    return Math.max(...this._map.values());
  }

  minCount(): number {
    return Math.min(...this._map.values());
  }
}

class Point implements HashKey {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  toString(): string {
    return `(${this.x},${this.y})`;
  }
}

class Grid {
  private values: HashMap<Point, number>;
  maxX: number;
  maxY: number;

  constructor(input: string[]) {
    this.values = new HashMap<Point, number>();
    this.maxY = input.length;
    this.maxX = input[0].split("").length;

    for (let y = 0; y < this.maxY; y++) {
      const row = input[y].split("");
      for (let x = 0; x < this.maxX; x++) {
        const value = parseInt(row[x], 10);
        this.values.set(new Point(x, y), value);
      }
    }
  }

  toString(): string {
    let output = "";
    for (let y = 0; y < this.maxY; y++) {
      for (let x = 0; x < this.maxX; x++) {
        const value = this.values.get(new Point(x, y)) ?? " ";
        output += value.toString();
      }
      output += "\n";
    }
    return output;
  }

  surround(point: Point, diagonal: boolean = false): Point[] {
    const {x, y} = point;
    const s = [
      [x - 1, y],
      [x + 1, y],
      [x, y - 1],
      [x, y + 1],
    ];
    if (diagonal) {
      s.push(...[[x - 1, y - 1],
        [x + 1, y + 1],
        [x - 1, y + 1],
        [x + 1, y - 1]]);
    }
    return s.filter(([nX, nY]) => nX >= 0 && nX < this.maxX && nY >= 0 && nY < this.maxY)
      .map(([nX, nY]) => new Point(nX, nY));
  }

  value(point: Point): number {
    return this.values.get(point)!;
  }

  set(point: Point, value: number) {
    this.maxX = Math.max(this.maxX, point.x + 1);
    this.maxY = Math.max(this.maxY, point.y + 1);
    this.values.set(point, value);
  }
}

export { HashMap, HashKey, HashSet, Counter, Grid, Point };
