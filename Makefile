#!/usr/bin/make -f

.PHONY: all clean

all: finvTable.data fsqrtTable.data

clean:
	$(RM) finvTable.data fsqrtTable.data

finvTable.data: fpu/VHDL/finvTable.py
	python $<

fsqrtTable.data: fpu/VHDL/fsqrtTable.py
	python $<
