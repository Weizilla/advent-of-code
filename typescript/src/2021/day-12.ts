import { Solution } from "../solution";
import { print } from "../utils";

class Cave {
  id: string;
  big: boolean;
  next: string[];

  constructor(id: string) {
    this.id = id;
    this.big = id.toUpperCase() === id;
    this.next = [];
  }

  addNext(id: string) {
    this.next.push(id);
  }
}

function isBig(id: string): boolean {
  return id === id.toUpperCase();
}

class Route {
  caves: Map<string, Cave>;
  path: string[];
  seen: Set<string>;

  constructor(caves: Map<string, Cave>, path: string[]) {
    this.caves = caves;
    this.path = [...path];
    this.seen = new Set<string>(path.filter(n => !isBig(n)));
  }

  addPath(next: string) {
    this.path.push(next);
    if (!isBig(next)) {
      this.seen.add(next);
    }
  }

  curr(): string {
    return this.path[this.path.length - 1];
  }

  next(): Route[] {
    const nextIds = this.caves.get(this.curr())!.next;
    const filtered = nextIds.filter(nextId => isBig(nextId) || (!isBig(nextId) && !this.seen.has(nextId)));
    return filtered.map(nextId => {
      const newRoute = new Route(this.caves, this.path);
      newRoute.addPath(nextId);
      return newRoute;
    });
  }

  toString(): string {
    return `[${JSON.stringify(this.path)}]`;
  }
}

class Route2 extends Route {
  twice: string | null;

  constructor(caves: Map<string, Cave>, path: string[], twice: string | null) {
    super(caves, path);
    this.twice = twice;
  }

  next(): Route2[] {
    const nextIds = this.caves.get(this.curr())!.next;
    const filtered = nextIds.filter(nextId => isBig(nextId) || (!isBig(nextId) && !this.seen.has(nextId)));
    const newRoutes = filtered.map(nextId => {
      const newRoute = new Route2(this.caves, this.path, this.twice);
      newRoute.addPath(nextId);
      return newRoute;
    });

    if (this.twice === null) {
      const filtered2 = nextIds.filter(nextId => !isBig(nextId) && this.seen.has(nextId) && nextId !== "start" && nextId !== "end");
      filtered2.forEach(nextId => {
        const newRoute = new Route2(this.caves, this.path, nextId);
        newRoute.addPath(nextId);
        newRoutes.push(newRoute);
      });
    }
    return newRoutes;
  }

  toString(): string {
    return `[${JSON.stringify(this.path)}] ${this.twice}`;
  }
}

class Day12 extends Solution {
  constructor(example?: number) {
    super(12, 2021, example);
  }

  part1(): number | string | undefined {
    const caves = this.parseCaves();

    let routes = [new Route(caves, ["start"])];
    let notEnded = routes.map(r => r.curr()).filter(c => c !== "end").length > 0;

    while (notEnded) {
      routes = this.findPath(caves, routes);
      notEnded = routes.map(r => r.curr()).filter(c => c !== "end").length > 0;
    }

    print(routes.map(r => r.path).sort().join("\n"));

    return routes.length;
  }

  private parseCaves() {
    const lines = this.readInput();
    const caves = new Map<string, Cave>();

    lines.forEach(line => {
      const [startId, endId] = [...line.split("-")];
      const start = caves.get(startId) || new Cave(startId);
      caves.set(startId, start);
      const end = caves.get(endId) || new Cave(endId);
      caves.set(endId, end);
      start.addNext(endId);
      end.addNext(startId);
    });

    return caves;
  }

  findPath<T extends Route>(caves: Map<string, Cave>, routes: T[]): T[] {
    const newRoutes: Route[] = [];
    routes.forEach(route => {
      const curr = route.curr();
      if (curr !== "end") {
        route.next().forEach(r => newRoutes.push(r));
      } else {
        newRoutes.push(route);
      }
    });

    return <T[]>newRoutes;
  }

  part2(): number | string | undefined {
    const caves = this.parseCaves();

    let routes = [new Route2(caves, ["start"], null)];
    let notEnded = routes.map(r => r.curr()).filter(c => c !== "end").length > 0;

    while (notEnded) {
      routes = this.findPath(caves, routes);
      notEnded = routes.map(r => r.curr()).filter(c => c !== "end").length > 0;
    }

    // print(routes.map(r => r.toString()).sort().join("\n"));

    return routes.length;
  }
}

(new Day12()).run();
