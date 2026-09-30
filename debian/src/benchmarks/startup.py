#!/usr/bin/python3
"""Compare startup time between two builds, interleaved.

    startup.py <nolto-root> <lto-root> <native|svm>

Startup is tens of milliseconds and process launch is noisy, so taking
the best of a run of samples from one binary and then the best of a run
from the other gives a different answer each time -- it measures how busy
the machine was during each run as much as anything else.  Sampling the
two alternately makes any drift fall on both equally.

Reports both the minimum and the median: the minimum is the least
disturbed launch, the median says what a user would typically see.
"""
import statistics, subprocess, sys, time

SAMPLES = 60
BINARIES = {"native": ("mit-scheme-x86-64", "mit-scheme"),
            "svm": ("mit-scheme-svm1-64le", "mit-scheme-svm")}

def main(nolto, lto, flavour):
    binname, libname = BINARIES[flavour]
    triplet = subprocess.run(["dpkg-architecture", "-qDEB_HOST_MULTIARCH"],
                             text=True, capture_output=True).stdout.strip() \
              or "x86_64-linux-gnu"
    roots = {"nolto": nolto, "lto": lto}

    def once(variant):
        root = roots[variant]
        t = time.perf_counter()
        subprocess.run([f"{root}/usr/bin/{binname}",
                        "--library", f"{root}/usr/lib/{triplet}/{libname}",
                        "--quiet", "--batch-mode"],
                       input="(exit)\n", text=True, capture_output=True)
        return (time.perf_counter() - t) * 1000

    for v in roots: once(v)                       # warm the page cache
    s = {v: [] for v in roots}
    for _ in range(SAMPLES):
        for v in ("nolto", "lto"):                # interleaved
            s[v].append(once(v))

    n, l = s["nolto"], s["lto"]
    print(f"{flavour} startup over {SAMPLES} interleaved samples")
    for label, f in (("min", min), ("median", statistics.median)):
        print(f"  {label:6s} no LTO {f(n):6.1f} ms   LTO {f(l):6.1f} ms"
              f"   {f(n)/f(l):.2f}x")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    main(*sys.argv[1:4])
