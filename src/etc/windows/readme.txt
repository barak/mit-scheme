How to build a windows release:

Get the i386 binary file and unpack it.

    cd mit-scheme-XX.YY\src
    etc\windows\compile-prepare.bat
    etc\windows\make-lib.bat
    etc\windows\build-band.bat

then, at the Scheme prompt:

    (load "../etc/windows/build-band")

Finally, edit the file "dist/scheme-inst.nsi" to reflect the new
version number and the location of the "mit-scheme-XX.YY" directory,
and compile it using NSIS.
