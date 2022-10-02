from typing import Union

from solution import Solution


class Day9(Solution):

    def __init__(self) -> None:
        super().__init__(9)

    def part1(self) -> Union[str, int]:
        total_players = 419
        max_steps = 71052

        curr_player = 0
        curr_marble_index = 0
        marbles: [int] = []
        step = 0
        scores = [0] * total_players

        for i in range(max_steps):
            to_insert = curr_marble_index + 2
            while to_insert > len(marbles) and len(marbles) != 0:
                to_insert -= len(marbles)

            if step % 23 == 0 and step > 0:
                scores[curr_player] += step
                remove_index = curr_marble_index - 7
                if remove_index < 0:
                    remove_index += len(marbles)
                old_value = marbles.pop(remove_index)
                scores[curr_player] += old_value
                curr_marble_index = remove_index

                if curr_marble_index >= len(marbles):
                    curr_marble_index -= len(marbles)
            else:
                marbles.insert(to_insert, step)
                curr_marble_index = to_insert

            # marbles_str = [f"({m})" if i == curr_marble_index else f"{m}" for i, m in enumerate(marbles)]
            # print(f"[{step}] [{curr_player}] {marbles_str}")

            step += 1
            curr_player = step % total_players

        print(f"scores {scores}")

        return max(scores)

    def part2(self) -> Union[str, int]:
        total_players = 419
        max_steps = 71052 * 100

        curr_player = 0
        curr_marble_index = 0
        marbles: [int] = []
        step = 0
        scores = [0] * total_players

        for i in range(max_steps):
            to_insert = curr_marble_index + 2
            while to_insert > len(marbles) and len(marbles) != 0:
                to_insert -= len(marbles)

            if step % 23 == 0 and step > 0:
                scores[curr_player] += step
                remove_index = curr_marble_index - 7
                if remove_index < 0:
                    remove_index += len(marbles)
                old_value = marbles.pop(remove_index)
                scores[curr_player] += old_value
                curr_marble_index = remove_index

                if curr_marble_index >= len(marbles):
                    curr_marble_index -= len(marbles)
            else:
                marbles.insert(to_insert, step)
                curr_marble_index = to_insert

            # marbles_str = [f"({m})" if i == curr_marble_index else f"{m}" for i, m in enumerate(marbles)]
            # print(f"[{step}] [{curr_player}] {marbles_str}")

            step += 1
            curr_player = step % total_players

        print(f"scores {scores}")

        return max(scores)


if __name__ == "__main__":
    Day9().run()
