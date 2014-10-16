#!/usr/bin/make -f

LIBRARYNAME = work
SOURCES = \
	serial.vhd kakeudon.vhd memory_controller.vhd register_file.vhd \
	alu.vhd io_rs232c.vhd core.vhd cpu.vhd \
	sramsim.vhd cpu_tb.vhd
# SOURCES = \
# 	serial.vhd cpu.vhd top.vhd \
# 	sramsim.vhd cpu_tb.vhd top_tb.vhd
TESTBENCHES = cpu_tb
# TESTBENCHES = cpu_tb top_tb

OBJS = $(SOURCES:.vhd=.o)

GHDLC = ghdl
GHDLFLAGS = \
	-fexplicit --ieee=synopsys --work=$(LIBRARYNAME) \
	#--std=93
GHDL_SIM_OPT = --stop-time=1ms

.PHONY: all test clean check-syntax $(TESTBENCHES)

all: $(OBJS)

$(OBJS): $(SOURCES)
	$(GHDLC) -a $(GHDLFLAGS) $^

test: $(TESTBENCHES)

$(TESTBENCHES): $(OBJS)
	$(GHDLC) -e $(GHDLFLAGS) $@
	# $(GHDLC) -r $(GHDLFLAGS) $@ $(GHDL_SIM_OPT) --vcd=$@.vcd
	$(GHDLC) -r $(GHDLFLAGS) $@ $(GHDL_SIM_OPT)

check-syntax:
	$(GHDLC) -s $(SOURCES)

clean :
	$(GHDLC) --clean
	$(RM) $(wildcard *.cf *.vcd)
