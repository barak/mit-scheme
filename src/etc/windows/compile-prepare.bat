cd microcode
call ntutl\wconfig.bat
wmake
cd ..\win32\dibutils
copy makefile.wcc makefile
wmake
cd ..\..\compiler
copy machines\i386\compiler.* .
cd ..
