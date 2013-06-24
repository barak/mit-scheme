@echo off
rd /s /q lib
md lib
copy etc\optiondb.scm lib

md lib\runtime
copy runtime\*.bci lib\runtime
for %%fn in (chrsyn cpress format mime-codec numint optiondb ordvec process rbtree regexp rexp rgxcmp syncproc wttree ystep) do copy runtime\%fn%.com lib\runtime

md lib\edwin
copy edwin\*.bci lib\edwin
for %%fn in (artdebug debian-changelog eystep lisppaste manual midas nntp paredit pasmod print pwedit pwparse snr sort techinfo telnet tximod verilog vhdl webster) do copy edwin\%fn%.com lib\edwin
copy etc\tutorial lib\edwin

for %%dir in (cref imail sf sos ssp star-parser xml) do (md lib\%dir% & copy %dir%\*.bci %dir%\*.com %dir%\*-w32.pkd lib\%dir%)
for %%dir in (imail sos ssp star-parser xml) do copy %dir%\load.scm lib\%dir%

md lib\compiler
md lib\compiler\machines
for %%dir in (back base fggen fgopt machines\i386 rtlbase rtlgen rtlopt) do (md lib\compiler\%dir% & copy compiler\%dir%\*.bci lib\compiler\%dir%)
