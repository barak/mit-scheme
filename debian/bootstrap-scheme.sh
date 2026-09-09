#!/bin/sh
# Build a stage-0 MIT/GNU Scheme from a prebuilt SVM band, for architectures
# where no MIT/GNU Scheme exists yet.
#
# MIT/GNU Scheme's compiler is written in Scheme, so building it needs a
# Scheme.  On a new architecture there is none, and the package would sit
# Build-Depends-Uninstallable for ever waiting for someone to seed it by hand.
#
# The way out is that the SVM -- upstream's own virtual machine -- executes
# bytecode, so a saved SVM band is portable to every machine of a given word
# size and byte order.  The band in debian/prebuilt is therefore usable
# anywhere, while the microcode that runs it is plain C and is compiled here
# from the source in this very package.  Together they are enough of a Scheme
# to compile the rest, which is then built entirely from source.
#
# usage: debian/bootstrap-scheme.sh SVM_ARCH BUILDDIR
#   writes BUILDDIR/run-build, a Scheme suitable for MIT_SCHEME_EXE

set -e

SVM_ARCH=${1:?usage: $0 SVM_ARCH BUILDDIR}
BUILDDIR=${2:?usage: $0 SVM_ARCH BUILDDIR}
PREBUILT=debian/prebuilt/${SVM_ARCH}

if [ ! -d "${PREBUILT}" ]; then
    echo "$0: no prebuilt Scheme for ${SVM_ARCH}; see debian/NOTES.md" >&2
    exit 1
fi

TOP=$(pwd)
rm -rf "${BUILDDIR}"
mkdir -p "${BUILDDIR}"

# Only the microcode: the top-level configure runs etc/create-makefiles.sh,
# which needs a working Scheme, whereas microcode/configure does not.
cp -a src "${BUILDDIR}/src"
( cd "${BUILDDIR}/src/microcode" \
  && ./configure --enable-native-code="${SVM_ARCH}" \
  && ${MAKE:-make} scheme )

# Present it the way an installed Scheme looks: the prebuilt library, and a
# wrapper that points the microcode at it.  Handing it a source tree instead
# gets a long way and then fails with "Unbound variable: ucode-primitive".
cp -a "${TOP}/${PREBUILT}" "${BUILDDIR}/lib"
cat > "${BUILDDIR}/run-build" <<'EOF'
#!/bin/sh
HERE=$(cd "$(dirname "$0")" && pwd)
exec "${HERE}/src/microcode/scheme" --library "${HERE}/lib" "$@"
EOF
chmod +x "${BUILDDIR}/run-build"

"${BUILDDIR}/run-build" --batch-mode --no-init-file \
    --eval '(begin (display microcode-id/compiled-code-type) (%exit 0))'
echo
