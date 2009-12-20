md lib
copy etc\optiondb.scm lib

md lib\runtime
copy runtime\*.bci lib\runtime
for %%fn in (chrsyn cpress format gdbm hashtb krypt mime-codec numint optiondb ordvec pgsql process rbtree regexp rexp rgxcmp syncproc wttree ystep) do copy runtime\%fn%.com lib\runtime

md lib\edwin
copy edwin\*.bci lib\edwin
for %%fn in (debian-changelog eystep lisppaste manual midas nntp paredit pasmod print pwedit pwparse snr sort techinfo telnet tximod verilog vhdl webster) do copy edwin\%fn%.com lib\edwin
