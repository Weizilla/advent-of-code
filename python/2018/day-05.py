from typing import Optional
from typing import Union

from solution import Solution


class Node:
    def __init__(self, value: any, prev: Optional["Node"] = None, next: Optional["Node"] = None) -> None:
        self.value = value
        self.prev = prev
        self.next = next

    def delete(self):
        if self.prev:
            self.prev.next = self.next

        if self.next:
            self.next.prev = self.prev


class LinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def add(self, value: any) -> None:
        if self.head:
            new_node = Node(value, prev=self.tail)
            self.tail.next = new_node
            self.tail = new_node
        else:
            self.head = Node(value)
            self.tail = self.head

    def delete(self, node: Node):
        node.delete()
        if self.head == node:
            self.head = node.next

        if self.tail == node:
            self.tail = node.prev

    def __str__(self):
        if not self.head:
            return

        all = []

        curr = self.head
        while True:
            all.append(curr.value)
            if curr == self.tail:
                break
            else:
                curr = curr.next

        return "".join(all)

    def __len__(self):
        return len(str(self))


class Day5(Solution):

    def __init__(self) -> None:
        super().__init__(5)

    def part1(self) -> Union[str, int]:
        input = list(self.read_input()[0])

        polymer = LinkedList()
        [polymer.add(i) for i in input]
        # print(polymer)

        changed = True
        while changed:
            changed = False
            curr = polymer.head
            curr_next = curr.next
            while curr and curr_next:
                if self.opposite(curr.value) == curr_next.value:
                    # print(" " + str(polymer))
                    polymer.delete(curr)
                    # print(" " + str(polymer))
                    polymer.delete(curr_next)
                    # print(" " + str(polymer))
                    # print("---")
                    changed = True
                    # break
                    curr = curr_next.next
                    curr_next = curr.next
                else:
                    curr = curr_next
                    curr_next = curr.next

            # print(polymer)

        return len(polymer)

    def opposite(self, letter: str) -> str:
        return letter.lower() if letter.isupper() else letter.upper()


# def part2(self) -> Union[str, int]:


if __name__ == "__main__":
    Day5().run()
