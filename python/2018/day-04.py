import re
from collections import Counter
from collections import defaultdict
from dataclasses import dataclass
from dataclasses import field
from typing import Optional
from typing import Union

from solution import Solution


@dataclass
class GuardSchedule:
    id: int
    schedule: dict[str, list[int]] = field(default_factory=lambda: defaultdict(list))
    minute_count: Counter = field(default_factory=Counter)


@dataclass
class GuardEvent:
    guard_id: Optional[int]
    date: str
    hour: int
    minute: int
    sleep: bool
    wake: bool


class Day4(Solution):

    def __init__(self) -> None:
        super().__init__(4)

    def part1(self) -> Union[str, int]:
        guard_schedules = self.build_guard_schedule()

        all_sleep_mins = []
        for guard in guard_schedules.values():
            sleep_mins = sum([len(s) for s in guard.schedule.values()])
            all_sleep_mins.append((sleep_mins, guard.id))

        all_sleep_mins.sort()

        max_sleep = all_sleep_mins[-1]
        max_sleep_id = max_sleep[1]
        most_sleep_min = guard_schedules[max_sleep_id].minute_count.most_common(1)[0][0]

        return max_sleep_id * most_sleep_min

    def build_guard_schedule(self) -> dict[int, GuardSchedule]:
        input = sorted(self.read_input())

        guard_schedules = {}
        curr_guard = None
        sleep = False
        sleep_start = None
        for event in [self.parse_line(i) for i in input]:
            if event.guard_id:
                if event.guard_id not in guard_schedules:
                    guard_schedules[event.guard_id] = GuardSchedule(event.guard_id)
                curr_guard = guard_schedules[event.guard_id]
            elif not sleep and event.sleep:
                sleep = True
                sleep_start = event.minute
            elif sleep and event.wake:
                sleep = False
                for i in range(sleep_start, event.minute):
                    curr_guard.schedule[event.date].append(i)
                    curr_guard.minute_count[i] += 1

        return guard_schedules

    def parse_line(self, line: str) -> GuardEvent:
        match = re.match("\[1518-(.*) (\d+):(\d+)\] (.*)", line)
        # print(match.groups())

        date = match.group(1)
        hour = int(match.group(2))
        minute = int(match.group(3))
        event = match.group(4)

        event_match = re.match("Guard #(\d+) begins shift", event)
        guard_id = int(event_match.group(1)) if event_match else None
        sleep = "asleep" in event
        wake = "wakes" in event

        return GuardEvent(guard_id, date, hour, minute, sleep, wake)

    def part2(self) -> Union[str, int]:
        guard_schedule = self.build_guard_schedule()

        most_slept_mins = []
        for g in guard_schedule.values():
            most_slept_at_min = g.minute_count.most_common(1)
            if most_slept_at_min:
                most_slept_mins.append(((most_slept_at_min[0][1], most_slept_at_min[0][0]), g.id))

        most_slept_mins.sort()
        most_min_guard = most_slept_mins[-1]
        return most_min_guard[0][1] * most_min_guard[1]


if __name__ == "__main__":
    Day4().run()
