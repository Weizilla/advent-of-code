import { Solution } from "../solution";
import { sum } from "../utils";

function hexToBin(input: string): string {
  const bins: string[] = [];
  for (const i of input.split("")) {
    parseInt(i, 16).toString(2)
      .padStart(4, "0")
      .split("")
      .forEach(b => bins.push(b));
  }
  return bins.join("");
}

function hexToDec(input: string): number {
  return parseInt(input, 16);
}

function binToDec(input: string): number {
  return parseInt(input, 2);
}

class Packet {
  pType: string;
  version: number;
  type: number;
  _length: number;
  subpackets: Packet[];

  constructor(pType: string, version: number, type: number, length: number) {
    this.pType = pType;
    this.type = type;
    this.version = version;
    this._length = length;
    this.subpackets = [];
  }

  addSubpacket(packet: Packet) {
    this.subpackets.push(packet);
  }

  length(): number {
    return this._length + sum(this.subpackets.map(p => p.length()));
  }

  toString(): string {
    return `${this.pType} ver=${this.version} type=${this.type} len=${this.length()}`;
  }
}

class Literal extends Packet {
  value: number;

  constructor(version: number, type: number, valueLen: number, value: number) {
    super("LIT", version, type, 6 + valueLen);
    this.value = value;
  }

  toString(): string {
    return `${super.toString()} value=${this.value}`;
  }
}

class Operator extends Packet {
  constructor(version: number, type: number) {
    super("OPT", version, type, 7);
  }
}

class Day16 extends Solution {
  constructor(example?: number) {
    super(16, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput().slice(4, 5);
    const sumVers = inputs.map(i => {
      const sumVer = this.runPart1(i);
      this.print(`${i} ${sumVer}`);
      return sumVer;
    });

    return sumVers[0];
  }

  runPart1(input: string): number {
    const bin = hexToBin(input);
    this.print(`Input ${input} ${this.dumpBin(bin)}`);
    const head = this.parsePacket(bin, 0);

    return this.sumVersions(head);
  }

  parsePacket(input: string, depth: number): Packet {
    const version = binToDec(input.slice(0, 3));
    const type = binToDec(input.slice(3, 6));
    const subpackets = input.slice(6, input.length);
    if (type === 4) {
      return this.parseLiteral(version, type, depth, subpackets);
    } else {
      return this.parseOperator(version, type, depth, subpackets);
    }
  }

  private parseLiteral(version: number, type: number, depth: number, rest: string): Literal {
    let start = 0;
    const digits: string[] = [];

    while (rest.slice(start, start + 1) === "1") {
      rest.slice(start + 1, start + 5).split("").forEach(c => digits.push(c));
      start += 5;
    }
    rest.slice(start + 1, start + 5).split("").forEach(c => digits.push(c));

    const value = binToDec(digits.join(""));
    const literal = new Literal(version, type, start + 5, value);
    this.print(literal, depth);
    return literal;
  }

  private parseOperator(version: number, type: number, depth: number, rest: string): Packet {
    const lengthId = rest.slice(0, 1);

    if (lengthId === "0") {
      let subLen = binToDec(rest.slice(1, 16));
      let remainder = rest.slice(16, rest.length);
      const packet = new Operator(version, type);
      this.print(`* ${packet} sublen=${subLen} ${this.dumpBin(remainder)}`, depth);

      while (subLen > 0) {
        const p = this.parsePacket(remainder, depth + 1);
        packet.addSubpacket(p);
        subLen -= p.length();
        remainder = remainder.slice(p.length(), remainder.length);
        this.print(`- ${packet} sublen=${subLen} ${this.dumpBin(remainder)}`, depth);
      }
      return packet;
    } else if (lengthId === "1") {
      let subNum = binToDec(rest.slice(1, 12));
      let remainder = rest.slice(12, rest.length);
      const packet = new Operator(version, type);
      this.print(`* ${packet} subnum=${subNum} ${this.dumpBin(remainder)}`, depth);

      while (subNum > 0) {
        const p = this.parsePacket(remainder, depth + 1);
        packet.addSubpacket(p);
        subNum--;
        remainder = remainder.slice(p.length(), remainder.length);
        this.print(`- ${packet} subnum=${subNum} ${this.dumpBin(remainder)}`, depth);
      }

      return packet;
    }

    throw Error(`Unsupported len id ${lengthId}`);
  }

  dumpBin(bin: string): string {
    let output = "";
    for (let i = 0; i < bin.length; i++) {
      if (i % 5 === 0) {
        output += `(${i})`;
      }
      output += bin.slice(i, i + 1);
    }
    return output;
  }

  private sumVersions(head: Packet): number {
    return head.version + sum(head.subpackets.map(p => this.sumVersions(p)));
  }

  part2(): number | string | undefined {
    return undefined;
  }
}

(new Day16(1)).run();
