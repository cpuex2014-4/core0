#!/usr/bin/make -f

LIBRARYNAME = work
TESTBENCHES = cpu_tb
include sources.mk

GHDLC = ghdl
GHDLFLAGS = \
	-fexplicit --ieee=synopsys --work=$(LIBRARYNAME) \
	--mb-comments
GHDL_SIM_OPT = --stop-time=500ms

.PHONY: all test clean check-syntax run_cpu_tb

all: $(TESTBENCHES)

test: run_cpu_tb

$(TESTBENCHES): work-obj93.cf $(SOURCES)
	$(GHDLC) -m $(GHDLFLAGS) $@

run_cpu_tb: cpu_tb finvTable.data fsqrtTable.data
	$(GHDLC) -r $(GHDLFLAGS) $< $(GHDL_SIM_OPT)

work-obj93.cf: sources.mk
	$(RM) $@
	for i in $(SOURCES); do \
		$(GHDLC) -i $(GHDLFLAGS) $$i; \
	done

check-syntax:
	$(GHDLC) -s $(SOURCES)

clean :
	$(GHDLC) --clean
	$(RM) finvTable.data fsqrtTable.data

finvTable.data: ../fpu/VHDL/finvTable.py
	python $<

fsqrtTable.data: ../fpu/VHDL/fsqrtTable.py
	python $<
