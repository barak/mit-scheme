#!/usr/bin/python3
"""Turn measure.sh output into a Markdown table of LTO vs non-LTO.

Reads lines of

    <variant> <flavour> <phase> <benchmark> <cpu-ms> <wall-ms> [<iterations>]

where the times are per iteration, and prints, per flavour and phase, the
two times and the speedup; a ratio above 1.00 means LTO is faster.

The "wall drift" column is how far wall-clock exceeded CPU time, i.e. how
much the machine was doing something else at the time.  Large values mean
the measurement should be distrusted rather than that LTO did anything.
"""
import collections, math, sys

# startup is deliberately absent: measured sequentially it is dominated
# by run-to-run drift, so startup.py does it interleaved instead.
PHASES = ["interp", "compile", "compiled"]
ORDER = ["-", "fib", "tak", "nqueens", "bignum", "flonum",
         "string", "list", "vector", "alloc"]

def fmt(x):
    return f"{x:.0f}" if x >= 100 else f"{x:.3g}"

def main(path):
    t = collections.defaultdict(dict)   # (flavour, phase, bench) -> {variant: (cpu, wall, iters)}
    for line in open(path):
        f = line.split()
        if len(f) not in (6, 7):
            continue
        variant, flavour, phase, bench = f[:4]
        try:
            cpu, wall = float(f[4]), float(f[5])
            iters = int(f[6]) if len(f) == 7 else 1
        except ValueError:
            continue
        t[(flavour, phase, bench)][variant] = (cpu, wall, iters)

    for flavour in ("native", "svm"):
        for phase in PHASES:
            rows = [(b, v) for (fl, ph, b), v in t.items()
                    if fl == flavour and ph == phase]
            if not rows:
                continue
            rows.sort(key=lambda r: ORDER.index(r[0]) if r[0] in ORDER else 99)
            print(f"\n### {flavour}, {phase}\n")
            print("| benchmark | no LTO (ms) | LTO (ms) | speedup | iterations | wall drift |")
            print("|---|---:|---:|---:|---:|---:|")
            ratios = []
            for bench, v in rows:
                if "nolto" not in v or "lto" not in v:
                    continue
                (n, nw, ni), (l, lw, li) = v["nolto"], v["lto"]
                ratio = (n / l) if l else float("nan")
                if l:
                    ratios.append(ratio)
                drift = max((nw - n) / n if n else 0, (lw - l) / l if l else 0)
                name = phase if bench == "-" else bench
                iters = "-" if ni == li == 1 else f"{ni}/{li}"
                print(f"| {name} | {fmt(n)} | {fmt(l)} | {ratio:.2f}x "
                      f"| {iters} | {drift*100:+.0f}% |")
            if len(ratios) > 1:
                gm = math.exp(sum(math.log(r) for r in ratios) / len(ratios))
                print(f"| **geometric mean** | | | **{gm:.2f}x** | | |")

if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "results.txt")
