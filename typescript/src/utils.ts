import chalk from "chalk";

function print(input: any, color: string = "black") {
  console.log(chalk.keyword(color)("%s"), input);
}

export { print };
