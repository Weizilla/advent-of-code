class HashSet<K> implements Iterable<K> {
  private _values: Map<string, K>;
  private readonly strFn: (k: K) => string;

  constructor(strFn: (k: K) => string);
  constructor(strFn: (k: K) => string, values?: Iterable<K>);
  constructor(strFn: (k: K) => string, values?: Array<K>) {
    this._values = new Map<string, K>();
    this.strFn = strFn;
    if (values) {
      for (const value of values) {
        this._values.set(strFn(value), value);
      }
    }
  }

  has(value: K): boolean {
    return this._values.has(this.strFn(value));
  }

  add(...values: K[]) {
    values.forEach(value => {
      this._values.set(this.strFn(value), value);
    });
  }

  values(): Array<K> {
    return Array.from(this._values.values());
  }

  delete(value: K) {
    this._values.delete(this.strFn(value));
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

class HashMap<K, V> {
  private _map: Map<string, V>;
  private _keys: Map<string, K>;
  private readonly strFn: (k: K) => string;

  constructor(strFn: (k: K) => string) {
    this._map = new Map<string, V>();
    this._keys = new Map<string, K>();
    this.strFn = strFn;
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
    return this._map.get(this.strFn(key));
  }

  set(key: K, value: V) {
    this._map.set(this.strFn(key), value);
    this._keys.set(this.strFn(key), key);
  }

  has(key: K): boolean {
    if (key === undefined) {
      throw Error("undefined key");
    }
    return this._keys.has(this.strFn(key));
  }

  delete(key: K) {
    this._map.delete(this.strFn(key));
    this._keys.delete(this.strFn(key));
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

class Tuple {
  x: number;
  y: number;
  z: number;

  constructor(x: number, y: number, z: number = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  scale(value: number): Tuple {
    return new Tuple(this.x * value, this.y * value, this.z * value);
  }

  sum(value: Tuple): Tuple {
    return new Tuple(this.x + value.x, this.y + value.y, this.z + value.z);
  }

  product(value: Tuple): number {
    return this.x * value.x + this.y * value.y + this.z * value.z;
  }

  distance(value: Tuple): number {
    return Math.abs(this.x - value.x) + Math.abs(this.y - value.y) + Math.abs(this.z - value.z);
  }

  matrixProduct(values: [Tuple, Tuple, Tuple]): Tuple {
    const nx = this.product(values[0]);
    const ny = this.product(values[1]);
    const nz = this.product(values[2]);
    return new Tuple(Math.round(nx), Math.round(ny), Math.round(nz));
  }

  static x(value: number) {
    return new Tuple(value, 0, 0);
  }

  static y(value: number) {
    return new Tuple(0, value, 0);
  }

  static z(value: number) {
    return new Tuple(0, 0, value);
  }

  toString(): string {
    return `(${this.x},${this.y},${this.z})`;
  }
}

class Grid<V> {
  private _values: HashMap<Tuple, V>;
  minX: number = 0;
  maxX: number = 0;
  minY: number = 0;
  maxY: number = 0;
  minZ: number = 0;
  maxZ: number = 0;

  constructor(fn: (a: string) => V, input?: string[] | null) {
    this._values = new HashMap<Tuple, V>(p => p.toString());
    if (input) {
      this.minX = 0;
      this.maxY = input.length;
      this.minY = 0;
      this.maxX = input[0].split("").length;

      for (let y = 0; y < this.maxY; y++) {
        const row = input[y].split("");
        for (let x = 0; x < this.maxX; x++) {
          const value = fn(row[x]);
          this._values.set(new Tuple(x, y), value);
        }
      }
    }
  }

  toString(padding: number = 0, defaultValue: V | null = null): string {
    let output = "";
    for (let y = this.minY - padding; y < this.maxY + padding; y++) {
      for (let x = this.minX - padding; x < this.maxX + padding; x++) {
        const value = this._values.get(new Tuple(x, y)) ?? (defaultValue || " ");
        output += `${value}`;
      }
      output += "\n";
    }
    return output;
  }

  surround(point: Tuple, diagonal: boolean = false, infinite: boolean = false, includeOrg: boolean = false): Tuple[] {
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
      .map(([nX, nY]) => new Tuple(nX, nY));
  }

  value(point: Tuple): V | null {
    return this._values.get(point) || null;
  }

  surroundValues(
    point: Tuple,
    diagonal: boolean = false,
    defaultValue: V | null = null,
    infinite: boolean = false,
    includeOrg: boolean = false,
  ): (V | null)[] {
    const s = this.surround(point, diagonal, infinite, includeOrg);
    return s.map(p => this.value(p) || defaultValue).filter(v => v !== null);
  }

  set(point: Tuple, value: V) {
    this.minX = Math.min(this.minX, point.x);
    this.maxX = Math.max(this.maxX, point.x + 1);
    this.minY = Math.min(this.minY, point.y);
    this.maxY = Math.max(this.maxY, point.y + 1);
    this.minZ = Math.min(this.minZ, point.z);
    this.maxZ = Math.max(this.maxZ, point.z + 1);
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

export { HashMap, HashSet, Counter, NumGrid, StringGrid, Tuple };
