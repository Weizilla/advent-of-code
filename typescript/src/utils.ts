import chalk from "chalk";

function print(input: any, padding: number = 0, color: string = "black") {
  console.log("".padStart(padding * 2, " ") + chalk.keyword(color)(input));
}

function sum(input: Iterable<number>) {
  return [...input].reduce((p, c) => p + c, 0);
}

function range(endInput: number, startInput: number = 0, includeEnd: boolean = false): number[] {
  const start = Math.trunc(startInput);
  const end = Math.trunc(endInput);

  if (start === end) {
    return [start];
  }

  const inc = start > end ? -1 : 1;
  const newEnd = includeEnd ? end + inc : end;
  const results = [];
  for (let i = start; i !== newEnd; i += inc) {
    results.push(i);
  }
  return results;
}

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

function sortNums(nums: number[]) {
  nums.sort((a, b) => a - b);
}

export { print, sum, range, hexToBin, hexToDec, binToDec, sortNums };
