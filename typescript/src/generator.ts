import { readdirSync, readFileSync, writeFileSync } from "fs";
import path from "path";
import { print } from "./utils";

function generate(year: number) {
  const yearFiles = readdirSync(path.join(__dirname, year.toString()))
    .filter(f => f.indexOf("day-") >= 0);

  const existingDays = yearFiles
    .map(n => n.replace("day-", "").replace(".ts", ""))
    .map(n => parseInt(n, 10));
  const nextDay = Math.max(...existingDays) + 1;
  const nextDayStr = nextDay.toString().padStart(2, "0");

  const templatePath = path.join(__dirname, "day-template.txt");
  const templateContents = readFileSync(templatePath, "utf-8");
  const newDayContents = templateContents
    .replace(/DAY/g, nextDay.toString())
    .replace("YEAR", year.toString());

  const newDayPath = path.join(__dirname, year.toString(), `day-${nextDayStr}.ts`);
  writeFileSync(newDayPath, newDayContents);
  print(`Create day file ${newDayPath}`);

  const inputPath = path.join(__dirname, year.toString(), "inputs", `day-${nextDayStr}-input.txt`);
  writeFileSync(inputPath, "");
  print(`Create day input file ${inputPath}`);

  const examplePath = path.join(__dirname, year.toString(), "inputs", `day-${nextDayStr}-example-1.txt`);
  writeFileSync(examplePath, "");
  print(`Create day example file ${examplePath}`);

  print(newDayPath);
}

generate(2021);
