import re
from dataclasses import dataclass
from dataclasses import field
from typing import Optional
from typing import Union

from solution import Solution


@dataclass(order=True)
class Node:
    id: str
    prev: ["Node"] = field(default_factory=list)
    next: ["Node"] = field(default_factory=list)

    def __str__(self) -> str:
        return f"id={self.id}"

    def __repr__(self) -> str:
        return f"id={self.id}"

    def __eq__(self, o: "Node") -> bool:
        return o and self.id == o.id

    def __hash__(self) -> int:
        return self.id.__hash__()

    @property
    def seconds(self) -> int:
        return ord(self.id) - ord("A") + 61


@dataclass()
class Worker:
    id: int
    node: Optional[Node] = field(default=None)
    seconds_left: int = field(default=0)


class Day7(Solution):

    def __init__(self) -> None:
        super().__init__(7)

    def part1(self) -> Union[str, int]:
        input = self.read_input()
        nodes = self.parse_input(input)

        start_nodes = sorted([n for n in nodes if not n.prev])

        path = []
        to_visit = set(start_nodes)

        while to_visit:
            curr = sorted(list(to_visit))[0]
            path.append(curr.id)
            to_visit.remove(curr)

            next_visit = [n for n in curr.next if not n.prev or all([np.id in path for np in n.prev])]

            to_visit = to_visit.union(next_visit)

        return "".join(path)

    def parse_input(self, input: [str]) -> [Node]:
        nodes = {}

        for line in input:
            match = re.match("Step ([A-Z]) must be finished before step ([A-Z]) can begin.", line)
            id = match.group(1)
            next_id = match.group(2)

            curr_node = nodes.setdefault(id, Node(id))
            next_node = nodes.setdefault(next_id, Node(next_id))

            curr_node.next.append(next_node)
            next_node.prev.append(curr_node)
        # print(nodes)
        return nodes.values()

    def part2(self) -> Union[str, int]:
        input = self.read_input()
        nodes = self.parse_input(input)

        start_nodes = sorted([n for n in nodes if not n.prev])

        path = []
        to_visit = set(start_nodes)

        curr_time = 0
        workers = [Worker(i) for i in range(5)]

        while to_visit:
            for worker in workers:
                if worker.node:
                    worker.seconds_left -= 1
                    if worker.seconds_left == 0:
                        path.append(worker.node.id)
                        to_visit.remove(worker.node)
                        next_visit = [n for n in worker.node.next if
                            not n.prev or all([np.id in path for np in n.prev])]
                        to_visit = to_visit.union(next_visit)
                        worker.node = None

                if not worker.node:
                    curr_working = [w.node for w in workers]
                    can_visit = [n for n in to_visit if n not in curr_working]
                    if can_visit:
                        curr = sorted(can_visit)[0]
                        worker.node = curr
                        worker.seconds_left = curr.seconds

            curr_time += 1

        print("".join(path))

        return curr_time - 1


if __name__ == "__main__":
    Day7().run()
