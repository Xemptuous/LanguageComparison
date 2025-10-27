#!/usr/bin/env python3
import statistics as stats
import subprocess as sp
from argparse import ArgumentParser, Namespace
from dataclasses import dataclass
from pathlib import Path
from time import perf_counter

# RUNS = 0  # recorded runs
# WARMUPS = 0  # unrecorded warmups
# SORT_BY = "min"
# DO_BUILD = False
# STR_PAD = 0


@dataclass
class Result:
    name: str
    times: list[float]
    mean: float
    stdev: float
    min: float


def run(cmd, cwd):
    start = perf_counter()
    try:
        sp.run(cmd, cwd=cwd, stdout=sp.DEVNULL, stderr=sp.DEVNULL, check=True)
    except sp.CalledProcessError:
        pass
    return perf_counter() - start


def maybe_build(dir: Path):
    print(f"{dir.name:>STR_PAD} Building...", end="", flush=True)
    build = dir / "build.sh"
    if build.exists() and build.is_file():
        sp.run(
            ["/bin/bash", "build.sh"],
            cwd=dir,
            stdout=sp.DEVNULL,
            stderr=sp.DEVNULL,
            check=True,
        )
    print("")


def bench_dir(
    lang: str, dirpath: Path, runs: int, warmups: int
) -> Result | None:
    runsh = dirpath / "run.sh"
    if not runsh.exists():
        return None

    # Warmups
    for _ in range(warmups):
        run(["/bin/bash", "run.sh"], dirpath)

    # Timed runs
    times = []
    for _ in range(runs):
        times.append(run(["/bin/bash", "run.sh"], dirpath))

    return Result(
        name=lang,
        times=times,
        mean=stats.fmean(times),
        stdev=stats.pstdev(times) if len(times) > 1 else 0.0,
        min=min(times),
    )


def main(args: Namespace):
    results: list[Result] = []
    for d in sorted(Path(__file__).parent.iterdir()):
        if d.name not in args.languages:
            continue
        proj_dir = d.joinpath(args.project)
        if d.is_dir() and (proj_dir / "run.sh").exists():
            if args.build:
                maybe_build(proj_dir)
            print(f"{d.name:>{args.str_pad}} ", end="", flush=True)
            try:
                res = bench_dir(d.name, proj_dir, args.runs, args.warmups)
                if res:
                    results.append(res)
                    print(
                        f"mean={res.mean:.6f}s  stdev={res.stdev:.6f}s  best={res.min:.6f}s"
                    )
            except sp.CalledProcessError as e:
                print(f"❌ failed (exit {e.returncode})")

    # Sort and print summary
    results.sort(key=lambda r: r.__getattribute__(args.sort_by))
    print("\nSorted by", args.sort_by, "(ascending):")
    for r in results:
        print(
            f"{r.name:>{args.str_pad}} {args.sort_by}={r.__getattribute__(args.sort_by):.6f}s  mean={r.mean:.6f}s  stdev={r.stdev:.6f}s  best={r.min:.6f}s"
        )


if __name__ == "__main__":
    dirs = {
        d.name
        for d in Path(__file__).parent.iterdir()
        if d.is_dir() and d.name[0] != "." and d.name[0].isupper()
    }
    str_pad = len(max(dirs, key=lambda d: len(d)))

    parser = ArgumentParser()
    parser.add_argument("project", type=lambda s: s.capitalize())
    parser.add_argument(
        "--languages", "-l", nargs="+", choices=dirs, default=dirs
    )
    parser.add_argument("--exclude", "-e", nargs="+", choices=dirs, default={})
    parser.add_argument("--runs", "-r", type=int, default=1)
    parser.add_argument("--warmups", "-w", type=int, default=0)
    parser.add_argument(
        "--sort-by", "-s", choices=["min", "mean"], default="min"
    )
    parser.add_argument(
        "--sort-dir", "-d", choices=["asc", "desc"], default="asc"
    )
    parser.add_argument("--build", "-b", action="store_true", default=False)

    args = parser.parse_args()

    args.str_pad = str_pad

    args.languages = set(args.languages).difference(args.exclude)

    main(args)
