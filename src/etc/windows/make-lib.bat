@echo off
md lib
copy etc\optiondb.scm lib

md lib\runtime
copy runtime\*.bci lib\runtime
for %%fn in (chrsyn cpress format gdbm hashtb krypt mime-codec numint optiondb ordvec pgsql process rbtree regexp rexp rgxcmp syncproc wttree ystep) do copy runtime\%fn%.com lib\runtime

md lib\edwin
copy edwin\*.bci lib\edwin
for %%fn in (debian-changelog eystep lisppaste manual midas nntp paredit pasmod print pwedit pwparse snr sort techinfo telnet tximod verilog vhdl webster) do copy edwin\%fn%.com lib\edwin

for %%dir in (cref imail sf sos ssp star-parser xml) do (md lib\%dir% & copy %dir%\*.bci %dir%\*.com %dir%\*-w32.pkd lib\%dir%)
for %%dir in (imail sos ssp star-parser xml) do copy %dir%\load.scm lib\%dir%

md lib\compiler
md lib\compiler\machines
for %%dir in (back base fggen fgopt machines\i386 rtlbase rtlgen rtlopt) do (md lib\compiler\%dir% & copy compiler\%dir%\*.bci lib\compiler\%dir%)
