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

export { HashMap, HashKey, HashSet, Counter };
