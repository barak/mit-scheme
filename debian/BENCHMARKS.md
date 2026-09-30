# LTO benchmarks

Ubuntu turns link-time optimisation on by default; Debian does not.
Debian release 12.1-13 includes a patch to build with LTO
(`0028-lto-type-mismatch`), but I thought it made sense to do a quick
benchmark and see if LTO was worth it.

These were done on amd64 aka X86_64.

Summary: **performance boost of around 10% on interpreted code, 2-3%
on compiled code, and a slightly faster startup.** Nothing slows down,
but some code volume increases slightly.

## What LTO might be expected to speed up

LTO optimises across the C translation units of the *microcode*,
meaning the interpreter, the garbage collector, and primitives. It
does not affect code the Scheme compiler generates, which is native
machine code in the `mit-scheme` package and SVM bytecode in
`mit-scheme-svm`. So any potential speedup would depend on how much
time is spent in compiled C:

| **Workload**       | **Where the Time Goes**               | **Possible LTO Speedup**               |
|--------------------|---------------------------------------|----------------------------------------|
| interpreted Scheme | `interp.c` and the primitives         | large                                  |
| compiled, native   | machine code from the Scheme compiler | edges only: primitives, allocation, GC |
| compiled, SVM      | `svm1-interp.c`                       | large in principle                     |

LTO can only speed up code where routines in one file call routines in
another. The “fast path” code in the bytecode (SVM) engine has
presumably been tuned to avoid that.

## The benchmarks

Nine simple programs, in `defs.scm`, each written to exercise a
different part of the runtime system. These are not meant to be
representative of any particular practical workload.

| **benchmark** | **workload**                                             |
|---------------|----------------------------------------------------------|
| `fib`         | fixnum arithmetic and non-tail recursion, `(fib 27)`     |
| `tak`         | deep recursion and argument shuffling, `(tak 20 14 6)`   |
| `nqueens`     | list traversal and `append`, 9 queens                    |
| `bignum`      | bignum multiplication, 9000! then one `remainder`        |
| `flonum`      | flonum arithmetic and boxing, 1.5M iterations            |
| `string`      | `string-upcase` and `number->string`, 200k iterations    |
| `list`        | `map` and `sort` over a 250k list                        |
| `vector`      | `vector-set!`/`vector-ref` over 2M elements              |
| `alloc`       | allocation churn and hence the GC, 2M four-element lists |

Times are measured with `process-time-clock` inside Scheme, so process
startup is excluded, and this should be reasonably invariant to other
loads on the machine. Times are reported as **best of three** runs,
because we want the least-disturbed run rather than an average amount
of interference. Wall clock is recorded alongside and reported as
“wall drift”, so you can tell if a measurement was taken while the
machine was busy.

`process-time-clock` has 10ms granularity on Linux (technically 1/HZ
and most distributions set HZ=100), which is coarse against benchmarks
that take tens of milliseconds. (There are much more accurate timers
available on modern machines, but MIT/GNU Scheme 12.1 does not expose
them.) Each benchmark is therefore calibrated first: the repeat count
is chosen, per binary, to take about a second, putting quantisation
under 1%. That is why times are reported per iteration, and why the
iteration counts differ between the two builds and between the phases.
Wildly divergent iteration counts should be expected, since compiled
code is two orders of magnitude faster than interpreted code, and
native back-end code is an order of magnitude faster than SVM back-end
code.

Four phases:

- **startup** -- load the band and exit. Wall clock, necessarily. This
  measures the cost before any Scheme runs, which does not show up in
  CPU time. Measured by `startup.py`, which alternates between the two
  builds rather than finishing one before starting the other;
  rationale below.
- **interp** -- the definitions loaded from source, so the benchmark
  runs in the microcode's interpreter.
- **compile** -- `(cf "defs")`, this times the Scheme compiler itself.
  This benchmark has the most realistic workload here, and is similar
  to a system build. This is calibrated like the others, since one
  compilation is only a few ticks of the clock.
- **compiled** -- the same benchmarks after compilation, recalibrated to
  far higher repeat counts.

## Reproducing

From the top of an unpacked source tree:

    debian/src/benchmarks/benchmark.sh [workdir]

This builds the package twice—once with `optimize=-lto` (the default
on Debian as of Fall 2026) and once with `optimize=+lto` (the default
on Ubuntu as of Fall 2026)—and then does all the benchmarks using both
flavours and prints tables like those below. It takes a couple of
hours.

`measure.sh` times one unpacked package on its own, `startup.py`
compares the startup of two of them, and `tabulate.py` renders the
results to a file.

## Results

Measured with Debian mit-scheme 12.1-13, amd64, Intel Core i7-14700,
gcc 16.2.0. Times are CPU milliseconds per iteration, best of three. A
ratio above 1.00 means LTO is faster. The “iterations” column is the
calibrated repeat count for each binary, and “wall drift” is how far
wall-clock exceeded CPU, so a disturbed measurement should be
apparent. Raw numbers go in `results-12.1-13-amd64.txt`.

Startup is an exception: it is based on wall clock time, since the
point is to measure the start-up time of the system from invocation to
ready to run Scheme. It is measured by `startup.py`, which alternates
between the two builds so that drift should be distributed roughly
equally between the two. Measuring in blocks had much more variance,
ranging from 0.61× to 1.50×.

### startup, interleaved

Wall clock, 60 alternating samples of each build, page cache warmed.

| flavour |        | ¬LTO (ms) | +LTO (ms) | speedup |
|---------|--------|----------:|----------:|--------:|
| native  | min    |      16.5 |      15.7 |   1.05× |
| native  | median |      20.6 |      20.2 |   1.02× |
| svm     | min    |      65.6 |      62.9 |   1.04× |
| svm     | median |      67.6 |      65.9 |   1.03× |

### native, interp

| benchmark          | ¬LTO (ms) | +LTO (ms) |   speedup | iterations | wall drift |
|--------------------|----------:|----------:|----------:|-----------:|-----------:|
| fib                |        97 |      85.5 |     1.14× |      10/11 |        +1% |
| tak                |       163 |       146 |     1.12× |        6/7 |        +0% |
| nqueens            |      60.6 |        55 |     1.10× |      16/18 |        +1% |
| bignum             |      17.2 |      17.7 |     0.97× |      57/57 |        +1% |
| flonum             |       485 |       430 |     1.13× |        2/2 |        +1% |
| string             |       460 |       420 |     1.10× |        2/2 |        +1% |
| list               |       202 |       184 |     1.10× |        5/5 |        +0% |
| vector             |      1370 |      1190 |     1.15× |          - |        -0% |
| alloc              |       680 |       605 |     1.12× |        1/2 |        +0% |
| **geometric mean** |           |           | **1.10×** |            |            |

### native, compile

| benchmark | ¬LTO (ms) | +LTO (ms) | speedup | iterations | wall drift |
|-----------|----------:|----------:|--------:|-----------:|-----------:|
| compile   |      33.8 |        33 |   1.02× |      29/30 |        +1% |

### native, compiled

| benchmark          | ¬LTO (ms) | +LTO (ms) |   speedup | iterations | wall drift |
|--------------------|----------:|----------:|----------:|-----------:|-----------:|
| fib                |      1.05 |      1.19 |     0.89× |    948/826 |        +1% |
| tak                |       1.6 |       1.4 |     1.14× |    595/692 |        +0% |
| nqueens            |     0.597 |     0.603 |     0.99× |  1625/1625 |        +1% |
| bignum             |      15.6 |      15.5 |     1.01× |      64/64 |        +0% |
| flonum             |      20.6 |      20.6 |     1.00× |      47/47 |        +1% |
| string             |       370 |       340 |     1.09× |        3/3 |        +0% |
| list               |       172 |       157 |     1.10× |        6/6 |        +0% |
| vector             |      66.7 |      61.9 |     1.08× |      15/16 |        +0% |
| alloc              |      14.5 |      14.3 |     1.01× |      55/69 |        +0% |
| **geometric mean** |           |           | **1.03×** |            |            |

### svm, interp

| benchmark          | ¬LTO (ms) | +LTO (ms) |   speedup | iterations | wall drift |
|--------------------|----------:|----------:|----------:|-----------:|-----------:|
| fib                |       118 |       102 |     1.15× |       8/10 |        +1% |
| tak                |       186 |       162 |     1.15× |        5/6 |        +0% |
| nqueens            |      73.8 |      63.1 |     1.17× |      13/16 |        +1% |
| bignum             |      17.4 |      16.9 |     1.02× |      57/59 |        +0% |
| flonum             |       690 |       600 |     1.15× |        1/2 |        +1% |
| string             |      3380 |      3360 |     1.01× |          - |        +0% |
| list               |      1050 |      1040 |     1.01× |          - |        +1% |
| vector             |      1600 |      1390 |     1.15× |          - |        +0% |
| alloc              |       800 |       670 |     1.19× |        1/2 |        +0% |
| **geometric mean** |           |           | **1.11×** |            |            |

### svm, compile

| benchmark | ¬LTO (ms) | +LTO (ms) | speedup | iterations | wall drift |
|-----------|----------:|----------:|--------:|-----------:|-----------:|
| compile   |       710 |       700 |   1.01× |          - |        -0% |

### svm, compiled

| benchmark          | ¬LTO (ms) | +LTO (ms) |   speedup | iterations | wall drift |
|--------------------|----------:|----------:|----------:|-----------:|-----------:|
| fib                |      26.8 |      26.5 |     1.01× |      37/37 |        +1% |
| tak                |      36.1 |      35.4 |     1.02× |      28/28 |        +1% |
| nqueens            |      11.1 |      10.9 |     1.02× |      90/91 |        +1% |
| bignum             |      15.5 |      15.6 |     0.99× |      64/64 |        +0% |
| flonum             |       272 |       270 |     1.01× |        4/4 |        +1% |
| string             |      3330 |      3200 |     1.04× |          - |        +0% |
| list               |      1030 |      1020 |     1.01× |          - |        +0% |
| vector             |       310 |       303 |     1.02× |        3/3 |        +0% |
| alloc              |       157 |       152 |     1.03× |        6/6 |        +1% |
| **geometric mean** |           |           | **1.02×** |            |            |

## Discussion

**Interpreted code gains about 10%**, consistently across both
flavours and across almost every benchmark. The SCode interpreter and
the primitives it calls are in different translation units, so this is
a best case for LTO.

**Compiled code gains 2–3%** (1.03× native, 1.02× SVM), close to
noise. For the native flavour that is expected: most work is being
done by machine code the Scheme compiler emitted, which is outside the
scope of the C compiler in general and LTO in particular. The
benchmarks that show an effect are the ones that spend much of their
time in primitives written in C: `string` (1.09×), `list` (1.10×),
`vector` (1.08×).

**The SVM's compiled case speeds up only 2% (1.02×).** Here is an
unverified guess as to what's going on. Every SVM instruction is
dispatched by C code in `svm1-interp.c`, so C-level optimisation is
important. However the SVM interpreter is one large dispatch loop
within a single translation unit, so gcc already optimises it without
LTO, and there is little cross-unit work left to do. LTO can get more
traction on the SCode interpreter as its hot paths are spread across
multiple files.

**Startup is slightly faster**, by 1.05× native and 1.04× SVM on the
least disturbed launch, 1.02× and 1.03× by median. That is a
millisecond or two, and it goes the opposite way to the obvious guess:
the LTO microcode is 6% larger (764656 bytes vs 719544 bytes) so one
might expect more to page in before anything runs. Whatever that costs
is evidently repaid by the startup path itself being faster. (This was
done using interleaved runs. Running blocks of one vs the other
yielded high variance, 0.85×, 0.86×, 0.61× and 1.50× on successive
runs, presumably due to other loads on the machine.)

**Compiler throughput is essentially unchanged** (1.02× native, 1.01×
SVM), which is of particular interest because it is the closest
approximation here to how the package is typically built and used. One
compilation of `defs.scm` takes a few tens of milliseconds, i.e., a
handful of ticks of a 10ms clock, so this is timed over a calibrated
run of repeats like everything else.

Note: we use geometric means of per-benchmark ratios.

## Conclusion

LTO is a positive but small win for interpreted code, has little
effect on compiled code either native or SVM, and makes startup very
slightly faster. This seems too small to justify explicitly enabling
it for distributions where it is not already the default.
