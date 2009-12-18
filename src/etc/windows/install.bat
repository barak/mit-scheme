copy microcode\scheme.exe c:\local\bin\mit-scheme.exe
del /s c:\local\mit-scheme
xcopy /e /y lib\*.* c:\local\mit-scheme
