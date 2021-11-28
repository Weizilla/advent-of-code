interface HashKey {
  toString(): string;
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

export { HashMap, HashKey };
