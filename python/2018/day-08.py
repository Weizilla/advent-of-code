from dataclasses import dataclass
from dataclasses import field
from typing import Union

from solution import Solution


@dataclass(order=True)
class Node:
    id: int
    length: int
    children: ["Node"] = field(default_factory=list)
    metadata: [int] = field(default_factory=list)

    @property
    def value(self) -> int:
        if self.children:
            return sum([self.children[m - 1].value for m in self.metadata if m <= len(self.children)])
        else:
            return sum(self.metadata)

    def __str__(self):
        return f"id={self.id} len={self.length} children={[n.id for n in self.children]} meta={self.metadata} value={self.value}"


class Day8(Solution):

    def __init__(self) -> None:
        super().__init__(8)
        self.curr_letter = 0

    def part1(self) -> Union[str, int]:
        input = [int(i) for i in self.read_input()[0].split(" ")]
        node, all_children = self.parse_input(input)
        sum_meta = sum([m for n in all_children for m in n.metadata])
        return sum(node.metadata) + sum_meta

    def next_id(self) -> str:
        id = chr(self.curr_letter + ord("A"))
        self.curr_letter += 1
        return id

    def parse_input(self, input) -> (Node, [Node]):
        id = self.next_id()
        num_children = input[0]
        num_metadata = input[1]

        if num_children == 0:
            metadata = input[2:2 + num_metadata]
            return Node(id, 2 + num_metadata, metadata=metadata), []

        children = []
        all_children = []
        for _ in range(num_children):
            all_child_len = sum([2 + len(n.metadata) for n in all_children])
            remainder = input[2 + all_child_len:]
            node, n_child = self.parse_input(remainder)
            all_children.extend([node] + n_child)
            children.append(node)

        all_child_len = sum([2 + len(n.metadata) for n in all_children])
        metadata = input[2 + all_child_len:2 + all_child_len + num_metadata]
        node = Node(id, 2 + all_child_len + num_metadata, children=children, metadata=metadata)

        return node, all_children

    # def part2(self) -> Union[str, int]:
    #     input = [int(i) for i in self.read_input()[0].split(" ")]
    #     node, _ = self.parse_input(input)
    #     return node.value


if __name__ == "__main__":
    Day8().run()
