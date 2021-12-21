interface HashKey {
  toString(): string;
}

class HashSet<K extends HashKey> implements Iterable<K> {
  private _values: Map<string, K>;

  constructor();
  constructor(values?: Iterable<K>);
  constructor(values?: Array<K>) {
    this._values = new Map<string, K>();
    if (values) {
      for (const value of values) {
        this._values.set(value.toString(), value);
      }
    }
  }

  has(value: K): boolean {
    return this._values.has(value.toString());
  }

  add(...values: K[]) {
    values.forEach(value => {
      this._values.set(value.toString(), value);
    });
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

  intersection(other: HashSet<K>): Set<K> {
    return new Set<K>(this.values().filter(v => other.has(v)));
  }

  [Symbol.iterator](): Iterator<K> {
    return this._values.values();
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
  z: number;

  constructor(x: number, y: number, z: number = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  scale(value: number): Point {
    return new Point(this.x * value, this.y * value, this.z * value);
  }

  sum(value: Point): Point {
    return new Point(this.x + value.x, this.y + value.y, this.z + value.z);
  }

  product(value: Point): number {
    return this.x * value.x + this.y * value.y + this.z * value.z;
  }

  matrixProduct(values: [Point, Point, Point]): Point {
    const nx = this.product(values[0]);
    const ny = this.product(values[1]);
    const nz = this.product(values[2]);
    return new Point(Math.round(nx), Math.round(ny), Math.round(nz));
  }

  static x(value: number) {
    return new Point(value, 0, 0);
  }

  static y(value: number) {
    return new Point(0, value, 0);
  }

  static z(value: number) {
    return new Point(0, 0, value);
  }


  toString(): string {
    return `(${this.x},${this.y},${this.z})`;
  }
}

class Grid<V> {
  private _values: HashMap<Point, V>;
  minX: number = 0;
  maxX: number = 0;
  minY: number = 0;
  maxY: number = 0;

  constructor(fn: (a: string) => V, input?: string[] | null) {
    this._values = new HashMap<Point, V>();
    if (input) {
      this.minX = 0;
      this.maxY = input.length;
      this.minY = 0;
      this.maxX = input[0].split("").length;

      for (let y = 0; y < this.maxY; y++) {
        const row = input[y].split("");
        for (let x = 0; x < this.maxX; x++) {
          const value = fn(row[x]);
          this._values.set(new Point(x, y), value);
        }
      }
    }
  }

  toString(padding: number = 0, defaultValue: V | null = null): string {
    let output = "";
    for (let y = this.minY - padding; y < this.maxY + padding; y++) {
      for (let x = this.minX - padding; x < this.maxX + padding; x++) {
        const value = this._values.get(new Point(x, y)) ?? (defaultValue || " ");
        output += `${value}`;
      }
      output += "\n";
    }
    return output;
  }

  surround(point: Point, diagonal: boolean = false, infinite: boolean = false, includeOrg: boolean = false): Point[] {
    const {x, y} = point;
    const s = [
      [x - 1, y],
      [x + 1, y],
      [x, y - 1],
      [x, y + 1],
    ];
    if (diagonal) {
      s.push(
        [x - 1, y - 1],
        [x + 1, y + 1],
        [x - 1, y + 1],
        [x + 1, y - 1],
      );
    }
    if (includeOrg) {
      s.push([x, y]);
    }

    s.sort(([x1, y1], [x2, y2]) => (y1 === y2 ? x1 - x2 : y1 - y2));

    return s.filter(([nX, nY]) => infinite || (nX >= this.minX && nX < this.maxX && nY >= this.minY && nY < this.maxY))
      .map(([nX, nY]) => new Point(nX, nY));
  }

  value(point: Point): V | null {
    return this._values.get(point) || null;
  }

  surroundValues(point: Point,
                 diagonal: boolean = false,
                 defaultValue: V | null = null,
                 infinite: boolean = false,
                 includeOrg: boolean = false,
  ): (V | null)[] {
    const s = this.surround(point, diagonal, infinite, includeOrg);
    return s.map(p => this.value(p) || defaultValue).filter(v => v !== null);
  }

  set(point: Point, value: V) {
    this.minX = Math.min(this.minX, point.x);
    this.maxX = Math.max(this.maxX, point.x + 1);
    this.minY = Math.min(this.minY, point.y);
    this.maxY = Math.max(this.maxY, point.y + 1);
    this._values.set(point, value);
  }

  values(): V[] {
    return this._values.values();
  }
}


class NumGrid extends Grid<number> {
  constructor(input?: string[]) {
    super(a => parseInt(a, 10), input);
  }
}

class StringGrid extends Grid<string> {
  constructor(input?: string[]) {
    super(a => a, input);
  }
}

export { HashMap, HashKey, HashSet, Counter, NumGrid, StringGrid, Point };
