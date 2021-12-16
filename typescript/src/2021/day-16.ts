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
  version: number;
  type: number;
  _length: number;
  subpackets: Packet[];

  constructor(version: number, type: number, length: number) {
    this.type = type;
    this.version = version;
    this._length = length;
    this.subpackets = [];
  }

  length(): number {
    return this._length + sum(this.subpackets.map(p => p.length()));
  }
}

class Literal extends Packet {
  value: number;

  constructor(version: number, type: number, valueLen: number, value: number) {
    super(version, type, 6 + valueLen);
    this.value = value;
  }
}

class Operator extends Packet {

  constructor(version: number, type: number, subPackets: Packet[]) {
    super(version, type, 7);
    this.subpackets = subPackets;
  }

}

class Day16 extends Solution {
  constructor(example?: number) {
    super(16, 2021, example);
  }

  part1(): number | string | undefined {
    const inputs = this.readInput().splice(4, 4);
    const sumVers = inputs.map(i => {
      const sumVer = this.runPart1(i);
      this.print(`${i} ${sumVer}`);
      return sumVer;
    });

    return sumVers[0];
  }

  runPart1(input: string): number {
    this.print(`Input ${input}`);
    const bin = hexToBin(input);
    const head = this.parsePacket(bin);

    return this.sumVersions(head);
  }

  parsePacket(input: string): Packet {
    const version = binToDec(input.slice(0, 3));
    const type = binToDec(input.slice(3, 6));
    this.print(`bin=${input} v=${version} type=${type}`);

    const rest = input.slice(6, input.length);
    if (type === 4) {
      return this.parseLiteral(version, type, rest);
    } else {
      return this.parseOperator(version, type, rest);
    }
  }

  private parseLiteral(version: number, type: number, rest: string): Literal {
    let start = 0;
    const digits: string[] = [];

    while (rest.slice(start, start + 1) === "1") {
      rest.slice(start + 1, start + 5).split("").forEach(c => digits.push(c));
      start += 5;
    }
    rest.slice(start + 1, start + 5).split("").forEach(c => digits.push(c));

    const value = binToDec(digits.join(""));
    const remainder = rest.slice(start + 5, rest.length);
    this.print(`LIT value ${value} rem ${remainder}`);
    return new Literal(version, type, start + 5, value);
  }

  private parseOperator(version: number, type: number, rest: string): Packet {
    const lengthId = rest.slice(0, 1);
    this.print(`Len id ${lengthId}`);

    if (lengthId === "0") {
      const subLenBin = rest.slice(1, 16);
      let subLen = binToDec(subLenBin);
      let remainder = rest.slice(16, rest.length);
      this.print(`OPT ver ${version} sub len ${subLen} rem ${remainder}`);
      const subPackets = [];
      while (subLen > 0) {
        const p = this.parsePacket(remainder);
        subPackets.push(p);
        subLen -= p.length();
        remainder = remainder.slice(p.length(), remainder.length);
      }
      return new Operator(version, type, subPackets);
    } else if (lengthId === "1") {
      const subNumBin = rest.slice(1, 12);
      let subNum = binToDec(subNumBin);
      let remainder = rest.slice(12, rest.length);
      this.print(`OPT ver ${version} sub num ${subNum} sub bin ${subNumBin} rem ${remainder}`);
      const subPackets = [];
      while (subNum > 0) {
        const p = this.parsePacket(remainder);
        subPackets.push(p);
        subNum--;
        remainder = remainder.slice(p.length(), remainder.length);
      }
      return new Operator(version, type, subPackets);
    }

    throw Error(`Unsupported len id ${lengthId}`);
  }

  private sumVersions(head: Packet): number {
    return head.version + sum(head.subpackets.map(p => this.sumVersions(p)));
  }

  part2(): number | string | undefined {
    return undefined;
  }
}

(new Day16(1)).run();
