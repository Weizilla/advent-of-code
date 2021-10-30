from pathlib import Path


def read_input(day: int) -> [str]:
    path = (Path(__file__).parent / f"inputs/day-{day:02}-input.txt").resolve()
    with open(path, "r") as f:
        return [r.strip() for r in f.readlines()]


def read_int_input(day: int) -> [int]:
    return [int(i) for i in read_input(day)]
