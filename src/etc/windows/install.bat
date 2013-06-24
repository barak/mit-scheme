copy microcode\scheme.exe c:\local\bin\mit-scheme.exe
del /q /s c:\local\mit-scheme
xcopy /e /i /y lib\*.* c:\local\mit-scheme
