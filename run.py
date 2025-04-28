#!/bin/python

import subprocess
import time
from argparse import ArgumentParser, Namespace
from pathlib import Path


def sp_run(path: Path, compile: bool):
    start_time = time.perf_counter()
    try:
        subprocess.run(
            path.joinpath("compile.sh" if compile else "run.sh"),
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        print(e.returncode, e.stderr)

    end_time = time.perf_counter()
    run_time = end_time - start_time
    print(f"{round(run_time * 1000)} ms")


def fstring(lang: str, proj: str, add: str):
    return f"{lang.ljust(12, '.')}{proj.ljust(15, '.')}" + add.ljust(10, ".")


def run(path: Path, args: Namespace, lang: str, proj: str):
    if args.compile:
        print(fstring(lang, proj, "Compiling..."), end="", flush=True)
        sp_run(path, True)
    if args.run:
        print(fstring(lang, proj, "Running....."), end="", flush=True)
        sp_run(path, False)


def is_doable(d: Path):
    return d.is_dir() and "." not in d.name and d.name[0].isupper()


if __name__ == "__main__":
    parser = ArgumentParser(description="Compile and/or run projects for languages")

    parser.add_argument("-c", "--compile", action="store_true")
    parser.add_argument("-r", "--run", action="store_true")
    parser.add_argument("-a", "--all", action="store_true")
    parser.add_argument("language", nargs="?", type=str.capitalize)
    parser.add_argument("project", nargs="?", type=str.capitalize)

    args = parser.parse_args()

    if not args.all and not args.language and not args.project:
        exit(0)

    if not args.compile and not args.run:
        args.compile = True
        args.run = True

    root = Path(__file__).parent
    if args.all:
        langs = sorted(d for d in root.iterdir() if is_doable(d))
        for dir in langs:
            for project in dir.iterdir():
                if is_doable(project):
                    run(project, args, dir.name, project.name)
    elif args.language and not args.project:
        for project in root.joinpath(args.language).iterdir():
            if is_doable(project):
                run(project, args, args.language, project.name)
    else:
        path = Path(__file__).parent.joinpath(args.language, args.project)
        run(path, args, args.language, args.project)
