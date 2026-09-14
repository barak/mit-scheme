# Packaging notes

Maintainer notes: how this package is built, and how to get it onto a
new architecture.

## Two flavours

The `mit-scheme` system has a native code back-end for some
architectures, and a portable bytecode backend (SVM = Scheme Virtual
Machine) which is roughly 20× slower. We generate two binary packages,
`mit-scheme` for the native code back-end and `mit-scheme-svm` for the
bytecode implementation. They both `Provide:mit-scheme-interpreter`
and use update-alternatives for `/usr/bin/mit-scheme`, native being
higher priority.

The native code back-end requires upstream to port to the instruction
set. Currently only amd64 and arm64 are supported.

The bytecode system, although portable, must still be bootstrapped
onto each new combination of word length (32/64-bit), endianity, and
memory layout (heap-in-low-memory vs not). Currently we are
bootstrapped onto both 32/64-bit, little endian, heap-in-low-memory.

Here is what is currently packaged:

| Package          | Back end     | Architecture             |
|------------------|--------------|--------------------------|
| `mit-scheme`     | native code  | amd64 arm64              |
| `mit-scheme-svm` | SVM bytecode | amd64 arm64 i386 ppc64el |

(Reverse dependency note: `scmutils` is the only package depending on
`mit-scheme`. It installs native code `.com` files into
`/usr/lib/<triplet>/mit-scheme` so requires the native code back-end.)

The build can bootstrap from either mit-scheme:native or
mit-scheme-svn:native. The former is preferred for efficiency, and
therefore the build dependency is set up to use that on architectures
where it is available.

    mit-scheme:native [amd64 arm64],
    mit-scheme-svm:native [!amd64 !arm64],

However alternatives are given to allow bootstrapping by seeding with
either, to make ports and new version easier.

Building either can be switched off using build profiles:

```sh
DEB_BUILD_PROFILES=pkg.mit-scheme.nosvm    dpkg-buildpackage
DEB_BUILD_PROFILES=pkg.mit-scheme.nonative dpkg-buildpackage
```

`debian/rules` figures out which flavours can be built from
`dh_listpackages`, making `debian/control` the single source of truth
for which architectures have native code back-ends.

## Architecture names

Upstream names its back ends its own way and its `configure` only
auto-detects x86, so `debian/rules` keeps the mapping in `BIN_ARCH`
(native: `x86-64`, `aarch64le`, `i386`) and `SVM_ARCH`
(`svm1-<bits><endian>`, from `DEB_HOST_ARCH_BITS`/`_ENDIAN`). Each
name controls three things: `--enable-native-code`, the installed
interpreter `mit-scheme-$ARCH-$VERSION`, and the `mit-scheme-$ARCH`
symlink the alternatives point at. `postinst`/`prerm` repeat the
mapping to work it out from `$DPKG_MAINTSCRIPT_ARCH` at run time.

Adding an architecture means an entry in each of those, plus the
`Architecture` field in `debian/control`, plus the corresponding list
of architectures in Build-depends:.

Note `src/microcode/confshared.h` only recognises i386, arm, aarch64,
powerpc, powerpc64 and x86_64, and refuses to compile without a match.
This means that s390x, riscv64, mips64el and loong64 cannot build the
bytecode interpreter. All the supported architectures are
little-endian.

## Cross-compiling

The build runs the Scheme it is built *with* to compile code for the
flavour being built. If the two disagree about compiled-code format,
the native path produces a band the new microcode cannot read —
`fasl-file-bad-data` when saving `lib/runtime.com`, most of the way
through the build. `debian/rules` therefore asks the build Scheme for
its `microcode-id/compiled-code-type` and passes
`--enable-cross-compiling` when it differs from the target.

## Bootstrapping a new architecture

MIT/GNU Scheme's compiler is written in Scheme, so the build needs a
Scheme, and a new architecture has none. Note also that a dpkg cross
build cannot help: the build must *run* the Scheme it just built, both
to dump the heap bands and in every plugin's `configure`, so the build
machine has to execute the target architecture's binaries.

There are two ways around it.

### Prebuilt bands (branch `debian-bootstrap`)

The SVM executes bytecode rather than machine code, so a saved band is
portable between machines that agree about object representation, while the
microcode that runs it is C and is compiled from this package's own source.
The `debian-bootstrap` branch carries such libraries in
`debian/prebuilt/svm1-<bits><endian>/`.

**Portable across what, exactly.** Not simply word size and byte order.
`src/microcode/object.h` branches on `HEAP_IN_LOW_MEMORY`, which selects
whether a Scheme object holds an absolute address or a base-relative one, so
it changes the representation of every object in a saved heap; a band can
only be loaded by a microcode that agrees. amd64, arm64, i386 and ppc64el
all define it, which is why one amd64-built 64-bit band serves amd64, arm64
and ppc64el. `__arm__` does not, so armhf can load neither a band nor FASL
files produced anywhere else:

    Pointer out of range: 0x40a211b8
    Error code 0x18 (fasl-file-bad-data).

Cross-building a band *for* armhf fails the same way, since the target
microcode still has to read host-written FASL at
`stamp_cross-finished`. armhf therefore cannot be bootstrapped by
either route and is left out of `debian/control`. Defining
`HEAP_IN_LOW_MEMORY` for `__arm__` would fix it if malloc there
returns addresses that fit in a Scheme datum, but the header says
leaving it undefined is the safe choice, so that is upstream's call.

`debian/rules` uses `debian/prebuilt/$(SVM_ARCH)` if it is there and
no Scheme is installed, so the same logic is harmless on the `debian`
branch, where the directory simply does not exist.

The bootstrap Scheme is only ever the compiler; everything shipped is
built from source.

To refresh the bands after a new upstream release — they must match
the source, or the build fails with `Bad compiled-code version in FASL
File`:

```sh
git checkout debian-bootstrap && git merge debian
# for each shape, build the SVM flavour and keep its installed library:
#   64-bit little-endian:  on any amd64 machine
#   32-bit little-endian:  same machine, CC="gcc -m32"
./configure --enable-native-code=svm1-32le --enable-cross-compiling \
            --enable-default-plugins=no --with-termcap=no
make && make install DESTDIR=/tmp/stage prefix=/usr
rm -rf debian/prebuilt/svm1-32le
cp -a /tmp/stage/usr/lib/mit-scheme-svm1-32le-*/ debian/prebuilt/svm1-32le
find debian/prebuilt -type f | sort > debian/source/include-binaries
git add -f debian/prebuilt debian/source/include-binaries
```

A complete installed library is wanted, not a hand-picked subset. A
band plus the `.pkd` files gets a long way and then fails with
`Unbound variable: ucode-primitive`, because the stage-0 Scheme ends
up pointed at a source tree rather than at a library.

### By hand, once

Alternatively, build the first binary for the new architecture
yourself and have it bootstrapped into the archive; ordinary buildd
builds take over afterwards. Seed from upstream's own SVM
distribution, which needs no existing Scheme:

    https://ftp.gnu.org/gnu/mit-scheme/stable.pkg/$VERSION/mit-scheme-$VERSION-svm1-64le.tar.gz

It carries `src/.native-release-marker`, which makes `configure` skip
the Scheme check and build only the microcode. **Apply
`debian/patches/*chacha*` to it first** — every upstream 12.1 tarball
has the unpatched `chacha.i` and dies on current toolchains with
`'_POSIX_C_SOURCE' redefined`, well into the build.
`--enable-default-plugins=no` keeps the seed small.

Then build this package against the seed, and rebuild once more
against the result, so that what you upload was built from archive
contents alone.

On a Debian porter box (`amdahl.debian.org` for arm64) that is a
persistent `schroot` session — `sessionid=$(schroot -b -c
sid-arm64-sbuild)`, `dd-schroot-cmd -c "$sessionid" apt-get ...` from
outside, `schroot -r` for a shell inside. Locally it needs
`qemu-user-static` and an emulated *native* chroot of the target
architecture, which is slow: reckon 10-15x. **Do not put your GPG key
on a porter box** — build with `-us -uc`, copy the results home,
`debsign` there.

Upstream's "Portable C" back end is not an option: no release has ever
shipped such a tarball, and `make liarc-dist` fails in 12.1 at
`cref-unx.bin`.

## Archive notes

When porting to a new architecture, either native code or byte code,
`mit-scheme` or `mit-scheme-svm` will be a new binary package. This
means you need to include them in the upload and they will go through
NEW. Once accepted, the binary package files will be discarded! Then,
in order to bootstrap on the buildds, you'll need to do binary uploads
of the new architecture-specific binaries. But uploaded binaries will
not progress to testing! So then you can either request a binary
rebuild, or just upload a new version.
