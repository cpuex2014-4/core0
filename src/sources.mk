SOURCES = \
	serial.vhd kakeudon.vhd memory_controller.vhd register_file.vhd \
	reservation_station.vhd reorder_buffer.vhd \
	load_store_buffer.vhd \
	fp_adder.vhd fp_multiplier.vhd fp_comparator.vhd fp_others.vhd \
	brancher.vhd alu.vhd io_rs232c.vhd core.vhd cpu.vhd \
	sramsim.vhd cpu_tb.vhd \
	../fpu/VHDL/fadd.vhd ../fpu/VHDL/fdiv.vhd ../fpu/VHDL/finv.vhd \
	../fpu/VHDL/fmul_stage1.vhd ../fpu/VHDL/fmul_stage2.vhd \
	../fpu/VHDL/fmul.vhd ../fpu/VHDL/fsqrt.vhd ../fpu/VHDL/ftoi.vhd \
	../fpu/VHDL/itof.vhd ../fpu/VHDL/kakeudon_fpu.vhd
