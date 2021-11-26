interface HashKey {
  toString(): string;
}

class HashMap<K extends HashKey, V> {
  map: Map<String, V>;

  constructor() {
    this.map = new Map();
  }

  get(key: K): V | undefined {
    return this.map.get(key.toString());
  }

  set(key: K, value: V) {
    this.map.set(key.toString(), value);
  }

  size() {
    return this.map.size;
  }

  toString(): string {
    return this.map.toString();
  }
}

export { HashMap, HashKey };
