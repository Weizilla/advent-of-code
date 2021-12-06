import chalk from "chalk";

function print(input: any, color: string = "black") {
  console.log(chalk.keyword(color)("%s"), input);
}

function sum(input: Iterable<number>) {
  return [...input].reduce((p, c) => p + c, 0);
}

function range(end: number, start: number = 0, includeEnd: boolean = false): number[] {
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

export { print, sum, range };
