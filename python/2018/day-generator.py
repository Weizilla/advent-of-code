import glob
from pathlib import Path

import colorama


class DayGenerator:

    def generate(self):
        existing_days = [int(f[len("day-"):-len(".py")]) for f in glob.glob("day-[0-9][0-9].py")]
        new_day = max(existing_days) + 1

        with open((Path(__file__).parent / "day-template.txt").resolve(), "r") as f:
            template_contents = f.read()
        new_contents = template_contents.replace("NUM", str(new_day))
        new_file_name = f"day-{new_day:02}.py"

        with open((Path(__file__).parent / new_file_name).resolve(), "w") as f:
            f.write(new_contents)

        print(f"{colorama.Fore.GREEN}Saved new file: {new_file_name}")


if __name__ == "__main__":
    gen = DayGenerator()
    gen.generate()
