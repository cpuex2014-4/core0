#!/usr/bin/make -f

LIBRARYNAME = work
FPU_SOURCES = fadd.vhd feq.vhd fle.vhd flt.vhd \
	      fmul_stage1.vhd fmul_stage2.vhd \
	      fmul.vhd fpu_core.vhd \
	      ftoi.vhd itof.vhd
CORE_SOURCES = \
	serial.vhd kakeudon.vhd memory_controller.vhd register_file.vhd \
	reservation_station.vhd reorder_buffer.vhd \
	alu.vhd io_rs232c.vhd core.vhd cpu.vhd \
	sramsim.vhd cpu_tb.vhd
SOURCES = $(FPU_SOURCES:%=../fpu/VHDL/%) $(CORE_SOURCES)
# SOURCES = \
# 	serial.vhd cpu.vhd top.vhd \
# 	sramsim.vhd cpu_tb.vhd top_tb.vhd
TESTBENCHES = cpu_tb
# TESTBENCHES = cpu_tb top_tb

OBJS = $(FPU_SOURCES:.vhd=.o) $(CORE_SOURCES:.vhd=.o)

GHDLC = ghdl
GHDLFLAGS = \
	-fexplicit --ieee=synopsys --work=$(LIBRARYNAME) \
	--mb-comments \
	#--std=93
GHDL_SIM_OPT = --stop-time=20ms

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
