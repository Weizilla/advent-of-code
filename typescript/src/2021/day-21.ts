import { Solution } from "../solution";
import { HashKey, HashMap } from "../collections";

function wrap(input: number, max: number): number {
  return input % max === 0 ? max : input % 10;
}

class DiceParameters implements HashKey {
  step: number;
  p1Score: number;
  p1Spot: number;
  p2Score: number;
  p2Spot: number;

  constructor(step: number, p1Score: number, p1Spot: number, p2Score: number, p2Spot: number) {
    this.step = step;
    this.p1Score = p1Score;
    this.p1Spot = p1Spot;
    this.p2Score = p2Score;
    this.p2Spot = p2Spot;
  }

  toString(): string {
    return `${this.step}|${this.p1Spot}|${this.p1Score}|${this.p2Spot}|${this.p2Score}`;
  }
}

class Day21 extends Solution {
  constructor(example?: number) {
    super(21, 2021, example);
  }

  part1(): number | string {
    const inputs = this.readInput().map(i => i.split(" ").slice(-1)[0]).map(i => parseInt(i, 10));
    let [p1Spot, p2Spot] = [...inputs];
    let p1Score = 0;
    let p2Score = 0;
    let dice = 1;
    this.print(`p1spot=${p1Spot} p2spot=${p2Spot}`);

    let step = 0;
    let diceResults = 0;
    while (p1Score < 1000 && p2Score < 1000) {
      diceResults += dice;
      this.print(`${step} dice=${dice} diceResult=${diceResults}`);
      if (step % 3 === 2) {
        if (step % 6 === 2) {
          this.print(`${step} p1spot=${p1Spot} p1score=${p1Score}`, 1);
          p1Spot += diceResults;
          p1Spot = wrap(p1Spot, 10);
          p1Score += p1Spot;
          this.print(`${step} p1spot=${p1Spot} p1score=${p1Score}`, 1);
        } else if (step % 6 === 5) {
          this.print(`${step} p2spot=${p2Spot} p2score=${p2Score}`, 1);
          p2Spot += diceResults;
          p2Spot = wrap(p2Spot, 10);
          p2Score += p2Spot;
          this.print(`${step} p2spot=${p2Spot} p2score=${p2Score}`, 1);
        }
        diceResults = 0;
      }

      step++;
      dice++;
      dice = wrap(dice, 100);
    }

    this.print(`${step} ${p1Score} ${p2Score}`);

    return Math.min(p1Score, p2Score) * step;
  }

  part2(): number | string {
    const inputs = this.readInput().map(i => i.split(" ").slice(-1)[0]).map(i => parseInt(i, 10));
    const [p1Spot, p2Spot] = [...inputs];
    const p1Score = 0;
    const p2Score = 0;
    const dices: number[] = [];
    this.print(`p1spot=${p1Spot} p2spot=${p2Spot}`);

    const newDiceParams = {
      step: 0,
      p1Score,
      p1Spot,
      p2Score,
      p2Spot,
    };

    const cache = new HashMap<DiceParameters, [number, number]>();

    const [p1Wins, p2Wins] = [...this.rollDice(dices, newDiceParams, cache)];

    this.print(`${p1Wins} ${p2Wins} ${p1Wins + p2Wins}`);

    return Math.max(p1Wins, p2Wins);
  }

  rollDice(dices: number[], {
    step,
    p1Score,
    p1Spot,
    p2Score,
    p2Spot,
  }: DiceParameters, cache: HashMap<DiceParameters, [number, number]>): [number, number] {
    let newP1Score = p1Score;
    let newP1Spot = p1Spot;
    let newP2Score = p2Score;
    let newP2Spot = p2Spot;
    let newStep = step;

    if (dices.length > 0) {
      const dice = dices.pop()!;
      dices.push(dice);

      if (newStep % 2 === 0) {
        newP1Spot = wrap(newP1Spot + dice, 10);
        newP1Score += newP1Spot;
      } else {
        newP2Spot = wrap(newP2Spot + dice, 10);
        newP2Score += newP2Spot;
      }

      if (newP1Score >= 21 || newP2Score >= 21) {
        const winner = newP1Score >= newP2Score ? "1" : "2";
        // this.print(`Player ${winner} wins dices=${dices} ${JSON.stringify({
        //   newStep,
        //   newP1Score,
        //   newP1Spot,
        //   newP2Score,
        //   newP2Spot,
        // })}`);
        return winner === "1" ? [1, 0] : [0, 1];
      }

      newStep++;
    }

    const newDiceParams = new DiceParameters(newStep, newP1Score, newP1Spot, newP2Score, newP2Spot);

    const cached = cache.get(newDiceParams);
    if (cached) {
      return cached;
    }

    const wins: [number, number] = [0, 0];
    for (const i of [1, 2, 3]) {
      for (const j of [1, 2, 3]) {
        for (const k of [1, 2, 3]) {
          const newDices = [...dices];
          newDices.push(i + j + k);
          const [p1, p2] = [...this.rollDice(newDices, newDiceParams, cache)];
          wins[0] += p1;
          wins[1] += p2;
        }
      }
    }

    cache.set(newDiceParams, wins);

    return wins;
  }
}


(new Day21()).run();
