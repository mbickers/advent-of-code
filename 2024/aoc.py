import pathlib
import subprocess


def input(day):
    path = pathlib.Path(f"day_{day:02}_input.txt")
    if not path.exists():
        subprocess.run(
            [
                f'curl https://adventofcode.com/2024/day/{day}/input --cookie "session=$(cat ../.token)" > day_{day:02}_input.txt'
            ],
            shell=True,
            check=True,
        )
    return path.read_text()
