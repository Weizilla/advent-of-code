import { Solution } from "../solution";
import { binToDec, hexToBin, sum } from "../utils";


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

  value(): number {
    return 0;
  }
}

class Literal extends Packet {
  _value: number;

  constructor(version: number, type: number, valueLen: number, value: number) {
    super("LIT", version, type, 6 + valueLen);
    this._value = value;
  }

  toString(): string {
    return `${super.toString()} value=${this._value}`;
  }

  value(): number {
    return this._value;
  }
}

class Operator extends Packet {
  constructor(version: number, type: number, subLen: number) {
    super("OPT", version, type, 7 + subLen);
  }

  value(): number {
    switch (this.type) {
      case 0:
        return sum(this.subpackets.map(p => p.value()));
      case 1:
        return this.subpackets.map(p => p.value()).reduce((a, b) => a * b);
      case 2:
        return Math.min(...this.subpackets.map(p => p.value()));
      case 3:
        return Math.max(...this.subpackets.map(p => p.value()));
      case 5:
        return this.subpackets[0].value() > this.subpackets[1].value() ? 1 : 0;
      case 6:
        return this.subpackets[0].value() < this.subpackets[1].value() ? 1 : 0;
      case 7:
        return this.subpackets[0].value() === this.subpackets[1].value() ? 1 : 0;
      default:
        throw Error();
    }
  }
}

class Day16 extends Solution {
  constructor(example?: number) {
    super(16, 2021, example);
  }

  part1(): number | string {
    const inputs = this.readInput();
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
      const packet = new Operator(version, type, 15);
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
      const packet = new Operator(version, type, 11);
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

  part2(): number | string {
    const inputs = this.readInput();
    const sumVers = inputs.map(i => {
      const sumVer = this.runPart2(i);
      this.print(`${i} ${sumVer}`);
      return sumVer;
    });

    return sumVers[0];
  }

  runPart2(input: string): number {
    const bin = hexToBin(input);
    this.print(`Input ${input} ${this.dumpBin(bin)}`);
    const head = this.parsePacket(bin, 0);
    return head.value();
  }
}

(new Day16()).run();
