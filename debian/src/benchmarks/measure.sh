#!/bin/bash
# Time one MIT/GNU Scheme binary.
#
#   measure.sh <unpacked-package-root> <native|svm> [label]
#
# <unpacked-package-root> is a directory into which the mit-scheme or
# mit-scheme-svm .deb has been extracted with "dpkg-deb -x".  Emits
#
#   [label] <phase> <benchmark> <milliseconds>
#
# one line per measurement; the time is the best of three runs, the best
# rather than the mean because we want the least-disturbed run, not the
# average amount of disturbance.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=${1:?usage: measure.sh <root> <native|svm> [label]}
FLAVOUR=${2:?usage: measure.sh <root> <native|svm> [label]}
LABEL=${3:-}
[ -n "$LABEL" ] && LABEL="$LABEL "

case $FLAVOUR in
  native) BIN=mit-scheme-x86-64;    LIB=mit-scheme ;;
  svm)    BIN=mit-scheme-svm1-64le; LIB=mit-scheme-svm ;;
  *) echo "measure.sh: flavour must be native or svm" >&2; exit 2 ;;
esac

TRIPLET=$(dpkg-architecture -qDEB_HOST_MULTIARCH 2>/dev/null || echo x86_64-linux-gnu)
B=$ROOT/usr/bin/$BIN
L=$ROOT/usr/lib/$TRIPLET/$LIB
[ -x "$B" ] || { echo "measure.sh: no $B" >&2; exit 1; }
[ -d "$L" ] || { echo "measure.sh: no $L" >&2; exit 1; }

W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
cp "$HERE/defs.scm" "$HERE/run.scm" "$W/"
cd "$W" || exit 1

run_suite () {                  # $1 = phase label
  echo '(begin (load "defs") (load "run"))' \
    | "$B" --library "$L" --quiet --batch-mode 2>/dev/null \
    | grep -E "^(fib|tak|nqueens|bignum|flonum|string|list|vector|alloc) " \
    | sed "s/^/$LABEL$1 /"
}

# 1. Startup: load the band and exit.  Recorded for reference only --
#    sequential sampling of a tens-of-milliseconds launch is dominated by
#    whatever else the machine was doing, and gave a different answer on
#    every run.  startup.py measures this properly, interleaved.
best=9999999
for _ in $(seq 15); do
  s=$(date +%s%N)
  echo '(exit)' | "$B" --library "$L" --quiet --batch-mode >/dev/null 2>&1
  e=$(date +%s%N)
  ms=$(( (e - s) / 1000000 ))
  [ $ms -lt $best ] && best=$ms
done
echo "${LABEL}startup - $best $best"   # wall only: this measures process launch

# 2. Interpreted: the definitions are loaded from source, so the workload
#    runs in the microcode's interpreter.
run_suite interp

# 3. The compiler itself, on those same definitions -- a realistic heavy
#    workload, and the one most like an actual build.
"$B" --library "$L" --quiet --batch-mode <<'EOF' 2>&1 \
  | grep -E "^compile |Unbound|error:" | sed "s/^/$LABEL/"
(load-option 'compiler)
;; One compilation is tens of milliseconds, i.e. a few ticks of a 10ms
;; clock; repeat it until the total is worth timing, as run.scm does.
(define (time-compiles n)
  (let ((p0 (process-time-clock)) (r0 (real-time-clock)))
    (let loop ((i 0)) (if (< i n) (begin (cf "defs") (loop (+ i 1)))))
    (cons (- (process-time-clock) p0) (- (real-time-clock) r0))))
(let loop ((n 1))
  (let ((t (time-compiles n)))
    (if (and (< (car t) 250) (< n 4096))
        (loop (* n 4))
        (let* ((reps (max 1 (round (/ (* n 1000) (max (car t) 1)))))
               (best (let again ((k 0) (bp #f) (br #f))
                       (if (= k 3) (cons bp br)
                           (let ((u (time-compiles reps)))
                             (again (+ k 1)
                                    (if (or (not bp) (< (car u) bp)) (car u) bp)
                                    (if (or (not br) (< (cdr u) br)) (cdr u) br)))))))
          (display "compile - ")
          (display (exact->inexact (/ (car best) reps))) (display " ")
          (display (exact->inexact (/ (cdr best) reps))) (display " ")
          (display reps) (newline)))))
EOF
[ -f defs.com ] || { echo "measure.sh: defs.com was not produced" >&2; exit 1; }

# 4. Compiled: defs.com now shadows defs.scm.  run.scm recalibrates, so
#    the repeat counts rise to match how much faster compiled code is.
run_suite compiled
