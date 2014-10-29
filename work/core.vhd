library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon_fpu.all;
use work.kakeudon.all;

entity core is
  port (
    -- Memory Controller
    mem_addr : out unsigned(29 downto 0);
    mem_data_write : out unsigned(31 downto 0);
    mem_data_read : in unsigned(31 downto 0);
    mem_we : out std_logic;
    -- RS-232C I/O Controller
    rs232c_recv_empty : in std_logic;
    rs232c_recv_top : in unsigned(7 downto 0);
    rs232c_recv_consume : out std_logic := '0';
    rs232c_send_full : in std_logic;
    rs232c_send_bottom : out unsigned(7 downto 0);
    rs232c_send_push : out std_logic := '0';
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);
end entity core;

architecture behavioral of core is
  signal rs_addr_id : unsigned(4 downto 0);
  signal rs_val_id_ex : unsigned(31 downto 0);
  signal rt_addr_id : unsigned(4 downto 0);
  signal rt_val_id_ex : unsigned(31 downto 0);
  signal rd_addr_wb : unsigned(4 downto 0);
  signal rd_val_wb : unsigned(31 downto 0);
  signal gpr_we_wb : std_logic;

  signal alu_control_id_ex : unsigned(5 downto 0);
  signal alu_in0_ex : unsigned(31 downto 0);
  signal alu_in1_ex : unsigned(31 downto 0);
  signal alu_out_ex : unsigned(31 downto 0);
  signal alu_out_ex_wb : unsigned(31 downto 0);
  signal alu_iszero_ex : std_logic;

  signal fs_addr_id : unsigned(4 downto 0);
  signal fs_val_id_ex : unsigned(31 downto 0);
  signal ft_addr_id : unsigned(4 downto 0);
  signal ft_val_id_ex : unsigned(31 downto 0);
  signal fd_addr_wb : unsigned(4 downto 0);
  signal fd_val_wb : unsigned(31 downto 0);
  signal fpr_we_wb : std_logic;

  signal fpu_control_id_ex : unsigned(5 downto 0);
  signal fpu_in0_ex : unsigned(31 downto 0);
  signal fpu_in1_ex : unsigned(31 downto 0);
  signal fpu_out_ex : unsigned(31 downto 0);
  signal fpu_condition_ex : std_logic;

  type cpu_state_t is (
    instruction_fetch,
    decode,
    execute,
    writeback);
  subtype stall_length_t is integer range 0 to 3;
  signal cpu_state : cpu_state_t := program_loading;
  signal cpu_stall_count : stall_length_t;

  signal enable_instruction_fetch : std_logic := '1';
  signal enable_decode : std_logic := '0';
  signal enable_execute : std_logic := '0';
  signal enable_writeback : std_logic := '0';

  signal stall_instruction_fetch : std_logic := '0';
  signal stall_decode : std_logic := '0';
  signal stall_execute : std_logic := '0';
  signal stall_writeback : std_logic := '0';

  signal loading_counter : unsigned(29 downto 0) := (others => '0');
  signal loading_word : unsigned(31 downto 0);
  subtype load_pos_t is integer range 0 to 4;
  signal load_pos : load_pos_t := 4;

  signal received_word : unsigned(31 downto 0);

  type instruction_memory_t is
    array(65535 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(29 downto 0) := (others => '0');
  signal program_counter_if_id : unsigned(29 downto 0);
  signal program_counter_plus1_if_id : unsigned(29 downto 0);
  signal program_counter_plus1_id_ex : unsigned(29 downto 0);
  signal program_counter_plus1_ex_wb : unsigned(29 downto 0);
  signal program_counter_plus1_plusimm_ex_wb : unsigned(29 downto 0);

  signal instruction_register_if_id : unsigned(31 downto 0);

  signal opcode_id : unsigned(5 downto 0);
  signal opcode_id_ex : unsigned(5 downto 0);
  signal funct_id : unsigned(5 downto 0);
  signal jump_immediate_val_id_ex : unsigned(25 downto 0);
  signal immediate_val_id_ex : unsigned(31 downto 0);

  type rs_io_mode_t is (rs_io_normal, rs_io_recv, rs_io_send);
  signal rs_io_mode_id_ex : rs_io_mode_t;
  type branch_target_t is (branch_target_normal,
                           branch_target_j, branch_target_b,
                           branch_target_alu,
                           branch_target_dontcare);
  signal branch_target_id_ex : branch_target_t;
  signal branch_by_cop1_id_ex : std_logic;
  signal branch_negate_id_ex : std_logic;
  type alu_src_a_t is
    (alu_src_a_rs, alu_src_a_shamt, alu_src_a_16, alu_src_a_dontcare);
  signal alu_src_a_id_ex : alu_src_a_t;
  signal alu_src_b_id_ex : std_logic;
  signal reg_dst_id_ex : std_logic;
  signal reg_write_id_ex : std_logic;
  signal reg_write_ex_wb : std_logic;
  signal reg_write_addr_id_ex : unsigned(4 downto 0);
  type reg_write_source_t is (
    reg_write_source_alu,
    reg_write_source_mem,
    reg_write_source_rs,
    reg_write_source_pc,
    reg_write_source_fpr,
    reg_write_source_dontcare);
  signal reg_write_source_id_ex : reg_write_source_t;
  signal reg_write_source_ex_wb : reg_write_source_t;

  signal fp_cond : std_logic;

  type fpu_op_t is (fpu_op_normal, fpu_op_dontcare);
  signal fpu_op_id_ex : fpu_op_t;
  signal fd_addr1_id_ex : unsigned(4 downto 0);
  signal fd_addr2_id_ex : unsigned(4 downto 0);
  signal fp_reg_dst_id_ex : std_logic;
  signal fp_reg_write_id_ex : std_logic;
  type fp_reg_write_source_t is (
    fp_reg_write_source_fs,
    fp_reg_write_source_gpr,
    fp_reg_write_source_fpu,
    fp_reg_write_source_dontcare);
  signal fp_reg_write_source_id_ex : fp_reg_write_source_t;
  signal fp_cond_write_id_ex : std_logic;

  signal cpu_stall_length_id_ex : stall_length_t;
begin

  state_control : process(clk, rst)
    variable next_cpu_state : cpu_state_t;
    variable next_cpu_stall_count : stall_length_t;
  begin
    if rst = '1' then
      cpu_state <= program_loading;
    elsif rising_edge(clk) then
      -- assert cpu_state = program_loading or cpu_state = memory_access_0
      --   report "cpu_state = " & cpu_state_t'image(cpu_state)
      --     severity note;
      next_cpu_state := cpu_state;
      next_cpu_stall_count := 0;
      case cpu_state is
      when program_loading =>
        if load_pos = 0 then
          if loading_word = (31 downto 0 => '1') then
            next_cpu_state := instruction_fetch;
          end if;
        end if;
      when instruction_fetch =>
        next_cpu_state := decode;
      when decode =>
        next_cpu_state := execute;
      when execute =>
        if rs_io_mode = rs_io_recv then
          if rs232c_recv_empty /= '1' then
            next_cpu_state := writeback;
          else
            next_cpu_state := stall;
          end if;
        elsif rs_io_mode = rs_io_send then
          if rs232c_send_full /= '1' then
            next_cpu_state := writeback;
          else
            next_cpu_state := stall;
          end if;
        elsif cpu_stall_length_id_ex = 0 then
          next_cpu_state := writeback;
        else
          next_cpu_state := stall;
          next_cpu_stall_count := cpu_stall_length_id_ex - 1;
        end if;
      when stall =>
        if rs_io_mode = rs_io_recv then
          if rs232c_recv_empty /= '1' then
            next_cpu_state := writeback;
          end if;
        elsif rs_io_mode = rs_io_send then
          if rs232c_send_full /= '1' then
            next_cpu_state := writeback;
          end if;
        elsif cpu_stall_count = 0 then
          next_cpu_state := writeback;
        else
          next_cpu_state := stall;
          next_cpu_stall_count := cpu_stall_count - 1;
        end if;
      when writeback =>
        next_cpu_state := instruction_fetch;
      end case;
      cpu_state <= next_cpu_state after 1 ns;
      cpu_stall_count <= next_cpu_stall_count after 1 ns;
    end if;
  end process state_control;

  enable_instruction_fetch <= '1';

  instruction_fetch_process: process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      assert TO_X01(enable_instruction_fetch) /= 'X'
        report "metavalue detected in enable_instruction_fetch"
          severity failure;
      assert TO_X01(stall_instruction_fetch) /= 'X'
        report "metavalue detected in stall_instruction_fetch"
          severity failure;
      if stall_instruction_fetch = '1' then
      elsif enable_instruction_fetch = '1' then
        assert TO_01(program_counter, 'X')(0) /= 'X'
          report "metavalue detected in program_counter"
            severity failure;
        -- report "program_counter/4 = " &
        --   integer'image(to_integer(program_counter(9 downto 0)));
        instruction_register_if_id <=
          instruction_memory(to_integer(program_counter(15 downto 0)));
        program_counter_if_id <= program_counter;
        program_counter_plus1_if_id <= program_counter + 1;
        enable_decode <= '1';
      else
        enable_decode <= '0';
      end if;
    end if;
  end process instruction_fetch_process;

  stall_instruction_fetch <= stall_decode;

  opcode_id <= instruction_register_if_id(31 downto 26);
  funct_id <= instruction_register_if_id(5 downto 0);
  rs_addr_id <= instruction_register_if_id(25 downto 21);
  rt_addr_id <= instruction_register_if_id(20 downto 16);
  -- rd_addr <= (others => '-') when
  --               reg_write_source = reg_write_source_dontcare else
  --            (others => '1') when reg_write_source = reg_write_source_pc else
  --            (others => 'X') when TO_X01(reg_dst) = 'X' else
  --            rd_addr2 when reg_dst = '1' else rd_addr1;

  fs_addr_id <= instruction_register_if_id(15 downto 11);
  ft_addr_id <= instruction_register_if_id(20 downto 16);
  -- fd_addr <= (others => 'X') when TO_X01(fp_reg_dst) = 'X' else
  --            fd_addr2 when fp_reg_dst = '1' else fd_addr1;

  decode_process: process(clk, rst)
    variable imm_signext : std_logic;
    variable next_alu_control : unsigned(5 downto 0);
    variable next_alu_src_a : alu_src_a_t;
    variable next_alu_src_b : std_logic;
    variable next_reg_write : std_logic;
    variable next_reg_write_addr : unsigned(4 downto 0);
    variable next_reg_write_source : reg_write_source_t;
    variable next_fpu_op : fpu_op_t;
    variable next_fp_reg_write : std_logic;
    variable next_fp_reg_dst : std_logic;
    variable next_fp_reg_write_source : fp_reg_write_source_t;
    variable next_fp_cond_write : std_logic;
    variable next_rs_io_mode : rs_io_mode_t;
    variable next_branch_target : branch_target_t;
    variable next_branch_by_cop1 : std_logic;
    variable next_branch_negate : std_logic;
    variable next_cpu_stall_length : stall_length_t;
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      assert TO_X01(enable_decode) /= 'X'
        report "metavalue detected in enable_decode"
          severity failure;
      assert TO_X01(stall_decode) /= 'X'
        report "metavalue detected in stall_decode"
          severity failure;
      if stall_decode = '1' then
      elsif enable_decode = '1' then
        assert TO_01(instruction_register_if_id, 'X')(0) /= 'X'
          report "metavalue detected in instruction_register_if_id; " &
                 "program_counter_if_id = " &
                 integer'image(to_integer(program_counter_if_id))
            severity failure;
        fd_addr1_id_ex <= instruction_register_if_id(10 downto 6);
        fd_addr2_id_ex <= instruction_register_if_id(15 downto 11);
        jump_immediate_val_id_ex <= instruction_register_if_id(25 downto 0);
        if instruction_register_if_id(31 downto 26) = "000000" then
          -- TODO: JR/JALR/SYSCALL/BREAK row
          reg_dst_id_ex <= '1';
          imm_signext := '-';
        -- elsif instruction_register_if_id(31 downto 26) = "000001" then
          -- TODO : REGIMM
        elsif instruction_register_if_id(31 downto 27) = "00001" then
          reg_dst_id_ex <= '-';
          imm_signext := '-';
        else
          reg_dst_id_ex <= '0';
          if instruction_register_if_id(31 downto 28) = "0011" then
            imm_signext := '0';
          else
            imm_signext := '1';
          end if;
        end if;
        immediate_val_id_ex(15 downto 0)
          <= instruction_register_if_id(15 downto 0);
        if TO_X01(imm_signext) = 'X' then
          immediate_val_id_ex(31 downto 16) <= (others => 'X');
        elsif imm_signext = '1' then
          immediate_val_id_ex(31 downto 16) <=
            (others => instruction_register_if_id(15));
        else
          immediate_val_id_ex(31 downto 16) <= (others => '0');
        end if;
        next_alu_control := (others => '-');
        next_alu_src_a := alu_src_a_dontcare;
        next_alu_src_b := '-';
        next_reg_write := '0';
        next_reg_write_addr := (others => '-');
        next_reg_write_source := reg_write_source_dontcare;
        next_fpu_op := fpu_op_dontcare;
        next_fp_reg_write := '0';
        next_fp_reg_dst := '-';
        next_fp_reg_write_source := fp_reg_write_source_dontcare;
        next_fp_cond_write := '0';
        next_rs_io_mode := rs_io_normal;
        next_branch_target := branch_target_normal;
        next_branch_by_cop1 := '-';
        next_branch_negate := '-';
        next_cpu_stall_length := 0;
        assert TO_01(opcode_id, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity failure;
        case opcode_t(to_integer(opcode_id)) is
        when OP_SPECIAL =>
          if instruction_register_if_id(5 downto 3) = "001" then
            next_alu_control :=
              to_unsigned(FUNCT_ADDU, next_alu_control'length);
            next_alu_src_a := alu_src_a_rs;
            next_alu_src_b := '0';
            next_branch_target := branch_target_alu;
          else
            next_alu_control := funct_id;
            if instruction_register_if_id(5 downto 2) = "0000" then
              next_alu_src_a := alu_src_a_shamt;
            else
              next_alu_src_a := alu_src_a_rs;
            end if;
            next_alu_src_b := '0';
            next_reg_write := '1';
            next_reg_write_addr := instruction_register_if_id(15 downto 11);
            next_reg_write_source := reg_write_source_alu;
          end if;
        when OP_J =>
          next_branch_target := branch_target_j;
        when OP_JAL =>
          next_reg_write := '1';
          next_reg_write_addr := "11111";
          next_reg_write_source := reg_write_source_pc;
          next_branch_target := branch_target_j;
        when OP_BEQ =>
          next_alu_control := to_unsigned(FUNCT_SUBU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '0';
          next_branch_target := branch_target_b;
          next_branch_by_cop1 := '0';
          next_branch_negate := '0';
        when OP_BNE =>
          next_alu_control := to_unsigned(FUNCT_SUBU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '0';
          next_branch_target := branch_target_b;
          next_branch_by_cop1 := '0';
          next_branch_negate := '1';
        when OP_ADDI =>
          next_alu_control := to_unsigned(FUNCT_ADD, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_ADDIU =>
          next_alu_control := to_unsigned(FUNCT_ADDU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_SLTI =>
          next_alu_control := to_unsigned(FUNCT_SLT, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_SLTIU =>
          next_alu_control := to_unsigned(FUNCT_SLTU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_ORI =>
          next_alu_control := to_unsigned(FUNCT_OR, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_XORI =>
          next_alu_control := to_unsigned(FUNCT_XOR, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_LUI =>
          next_alu_control := to_unsigned(FUNCT_SLL, next_alu_control'length);
          next_alu_src_a := alu_src_a_16;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_alu;
        when OP_COP1 =>
          case cop1_fmt_t(
                to_integer(instruction_register_if_id(25 downto 21))) is
          when COP1_FMT_MFC1 => -- MFC1
            next_reg_write := '1';
            next_reg_write_addr := instruction_register_if_id(20 downto 16);
            next_reg_write_source := reg_write_source_fpr;
          when COP1_FMT_MTC1 => -- MTC1
            next_fp_reg_write := '1';
            next_fp_reg_dst := '1';
            next_fp_reg_write_source := fp_reg_write_source_gpr;
          when COP1_FMT_BC => -- BC1F/BC1T
            next_branch_target := branch_target_b;
            next_branch_by_cop1 := '1';
            next_branch_negate := not instruction_register_if_id(16);
          when COP1_FMT_S => -- Single-precision
            case cop1_funct_t(to_integer(funct_id)) is
            when COP1_FUNCT_ADD => -- add.s
              next_fpu_op := fpu_op_normal;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fpu;
              next_cpu_stall_length := 1;
            when COP1_FUNCT_SUB => -- sub.s
              next_fpu_op := fpu_op_normal;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fpu;
              next_cpu_stall_length := 1;
            when COP1_FUNCT_MUL => -- mul.s
              next_fpu_op := fpu_op_normal;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fpu;
              next_cpu_stall_length := 1;
            when COP1_FUNCT_MOV => -- mov.s
              -- TODO
              -- fpu_op <= fpu_op_dontcare;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fs;
            when COP1_FUNCT_CVT_W => -- cvt.w.s
              next_fpu_op := fpu_op_normal;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fpu;
              next_cpu_stall_length := 3;
            when COP1_FUNCT_C_EQ => -- c.eq.s
              next_fpu_op := fpu_op_normal;
              next_fp_cond_write := '1';
            when COP1_FUNCT_C_OLT => -- c.olt.s
              next_fpu_op := fpu_op_normal;
              next_fp_cond_write := '1';
            when COP1_FUNCT_C_OLE => -- c.ole.s
              next_fpu_op := fpu_op_normal;
              next_fp_cond_write := '1';
            when others =>
              report "fp_inst.s : unknown funct " &
                integer'image(to_integer(funct_id))
                severity failure;
              next_reg_write := '-';
              next_fp_reg_write := '-';
              next_branch_target := branch_target_dontcare;
              next_fp_cond_write := '-';
            end case;
          when COP1_FMT_W => -- Fixed-point Word
            case cop1_funct_t(to_integer(funct_id)) is
            when COP1_FUNCT_CVT_S => -- cvt.s.w
              next_fpu_op := fpu_op_normal;
              next_fp_reg_write := '1';
              next_fp_reg_dst := '0';
              next_fp_reg_write_source := fp_reg_write_source_fpu;
              next_cpu_stall_length := 3;
            when others =>
              report "fp_inst.w : unknown funct " &
                integer'image(to_integer(funct_id))
                severity failure;
              next_reg_write := '-';
              next_fp_reg_write := '-';
              next_branch_target := branch_target_dontcare;
              next_fp_cond_write := '-';
            end case;
          when others =>
            report "COP1 : unknown fmt " &
              integer'image(to_integer(instruction_register_if_id(25 downto 21)))
              severity failure;
            next_reg_write := '-';
            next_fp_reg_write := '-';
            next_branch_target := branch_target_dontcare;
            next_fp_cond_write := '-';
          end case;
        when OP_LW =>
          next_alu_control := to_unsigned(FUNCT_ADDU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_mem;
          next_cpu_stall_length := 2;
        when OP_SW =>
          next_alu_control := to_unsigned(FUNCT_ADDU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '1';
          next_cpu_stall_length := 2;
        when OP_RRB =>
          next_alu_control := to_unsigned(FUNCT_ADDU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '0';
          next_reg_write := '1';
          next_reg_write_addr := instruction_register_if_id(20 downto 16);
          next_reg_write_source := reg_write_source_rs;
          next_rs_io_mode := rs_io_recv;
        when OP_RSB =>
          next_alu_control := to_unsigned(FUNCT_ADDU, next_alu_control'length);
          next_alu_src_a := alu_src_a_rs;
          next_alu_src_b := '0';
          next_rs_io_mode := rs_io_send;
        when others =>
          report "unknown opcode " &
            integer'image(to_integer(opcode_id))
            severity failure;
          next_reg_write := '-';
          next_fp_reg_write := '-';
          next_branch_target := branch_target_dontcare;
          next_fp_cond_write := '-';
        end case;
        alu_control_id_ex <= next_alu_control;
        alu_src_a_id_ex <= next_alu_src_a;
        alu_src_b_id_ex <= next_alu_src_b;
        reg_write_id_ex <= next_reg_write;
        reg_write_addr_id_ex <= next_reg_write_addr;
        reg_write_source_id_ex <= next_reg_write_source;
        fpu_op_id_ex <= next_fpu_op;
        fp_reg_write_id_ex <= next_fp_reg_write;
        fp_reg_dst_id_ex <= next_fp_reg_dst;
        fp_reg_write_source_id_ex <= next_fp_reg_write_source;
        fp_cond_write_id_ex <= next_fp_cond_write;
        rs_io_mode_id_ex <= next_rs_io_mode;
        branch_target_id_ex <= next_branch_target;
        branch_by_cop1_id_ex <= next_branch_by_cop1;
        branch_negate_id_ex <= next_branch_negate;
        cpu_stall_length_id_ex <= next_cpu_stall_length;

        opcode_id_ex <= opcode_id;
        program_counter_plus1_id_ex <= program_counter_plus1_if_id;

        enable_execute <= '1';
      else
        enable_execute <= '0';
      end if;
    end if;
  end process decode_process;

  stall_decode <= stall_execute;

  alu_in0_ex <= rs_val_id_ex when alu_src_a_id_ex = alu_src_a_rs else
                to_unsigned(16, alu_in0_ex'length)
                  when alu_src_a_id_ex = alu_src_a_16 else
                (31 downto 5 => '0') & immediate_val_id_ex(10 downto 6)
                  when alu_src_a_id_ex = alu_src_a_shamt else
                (others => 'X');

  alu_in1_ex <= (others => 'X') when TO_X01(alu_src_b_id_ex) = 'X' else
                rt_val_id_ex when alu_src_b_id_ex = '0' else
                immediate_val_id_ex;

  -- fpu_in0_ex <= ft_val_id_ex;
  -- fpu_in1_ex <= fs_val_id_ex;
  -- TODO: metavalue
  fpu_in0_ex <=
    (others => '1') when TO_01(fs_val_id_ex, 'X')(0) = 'X' else fs_val_id_ex;
  fpu_in1_ex <=
    (others => '1') when TO_01(ft_val_id_ex, 'X')(0) = 'X' else ft_val_id_ex;

  execute_process: process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      assert TO_X01(enable_execute) /= 'X'
        report "metavalue detected in enable_execute"
          severity failure;
      assert TO_X01(stall_execute) /= 'X'
        report "metavalue detected in stall_execute"
          severity failure;
      if stall_execute = '1' then
      elsif enable_execute = '1' then
        assert TO_01(opcode_id_ex, 'X')(0) /= 'X'
          report "metavalue detected in opcode_id_ex"
            severity failure;
        case opcode_t(to_integer(opcode_id_ex)) is
        when OP_LW =>
          assert alu_out_ex(1 downto 0) = "00"
            report "OP_LW: memory alignment check failed"
              severity failure;
          mem_data_write <= (others => '-');
          mem_we <= '0';
        when OP_SW =>
          assert alu_out_ex(1 downto 0) = "00"
            report "OP_SW: memory alignment check failed"
              severity failure;
          mem_data_write <= rt_val_id_ex;
          mem_we <= '1';
        when others =>
          mem_data_write <= (others => '-');
          mem_we <= '0';
        end case;
        mem_addr <= alu_out_ex(31 downto 2);
        program_counter_plus1_plusimm_ex_wb <=
          program_counter_plus1_id_ex + immediate_val_id_ex(29 downto 0);
        program_counter_plus1_ex_wb <= program_counter_plus1_id_ex;
        reg_write_ex_wb <= reg_write_id_ex;
        reg_write_source_ex_wb <= reg_write_source_id_ex;
        alu_out_ex_wb <= alu_out_ex;
      elsif cpu_state = stall then
        mem_data_write <= (others => '-');
        mem_we <= '0';
        mem_addr <= (others => '-');
      else
        mem_data_write <= (others => '-');
        mem_we <= '0';
        mem_addr <= (others => '-');
        program_counter_plus1_plusimm_ex_wb <= (others => '-');
        reg_write_ex_wb <= '-';
        reg_write_source_ex_wb <= reg_write_source_dontcare;
        alu_out_ex_wb <= (others => '-');
      end if;
    end if;
  end process execute_process;

  memory_access_process: process(clk, rst)
    variable next_rs232c_recv_consume : std_logic;
    variable next_rs232c_send_bottom : unsigned(7 downto 0);
    variable next_rs232c_send_push : std_logic;
  begin
    if rst = '1' then
      rs232c_recv_consume <= '0';
      rs232c_send_bottom <= (others => '-');
      rs232c_send_push <= '0';
      loading_counter <= (others => '0');
      load_pos <= 4;
    elsif rising_edge(clk) then
      next_rs232c_recv_consume := '0';
      next_rs232c_send_bottom := (others => '-');
      next_rs232c_send_push := '0';
      if cpu_state = program_loading then
        if load_pos = 0 then
          load_pos <= 4;
          if loading_word = (31 downto 0 => '1') then
            loading_counter <= (others => '0');
          else
            instruction_memory(to_integer(loading_counter(15 downto 0)))
              <= loading_word;
            loading_counter <= loading_counter + 1;
          end if;
        elsif rs232c_recv_empty /= '1' then
          next_rs232c_recv_consume := '1';
          case load_pos is
          when 4 =>
            loading_word(31 downto 24) <= rs232c_recv_top;
            load_pos <= 3;
          when 3 =>
            loading_word(23 downto 16) <= rs232c_recv_top;
            load_pos <= 2;
          when 2 =>
            loading_word(15 downto 8) <= rs232c_recv_top;
            load_pos <= 1;
          when 1 =>
            loading_word(7 downto 0) <= rs232c_recv_top;
            load_pos <= 0;
          when 0 =>
          end case;
        end if;
      elsif cpu_state = memory_access_0 then
        case rs_io_mode is
        when rs_io_normal =>
        when rs_io_recv =>
          -- read word from RS-232C, blocking
          if rs232c_recv_empty /= '1' then
            next_rs232c_recv_consume := '1';
            received_word <= (31 downto 8 => '0') & rs232c_recv_top;
          end if;
        when rs_io_send =>
          -- send word into RS-232C, blocking
          if rs232c_send_full /= '1' then
            -- report "sending : " &
            --   integer'image(to_integer(alu_out(7 downto 0)));
            next_rs232c_send_bottom := alu_out(7 downto 0);
            next_rs232c_send_push := '1';
          end if;
        end case;
      end if;
      rs232c_recv_consume <= next_rs232c_recv_consume;
      rs232c_send_bottom <= next_rs232c_send_bottom;
      rs232c_send_push <= next_rs232c_send_push;
    end if;
  end process memory_access_process;

  stall_execute <= stall_writeback;

  gpr_we_wb <= reg_write_ex_wb when cpu_state = writeback else '0';
  rd_val_wb <=
    (others => '-') when cpu_state /= writeback else
    alu_out_ex_wb when reg_write_source_ex_wb = reg_write_source_alu else
    mem_data_read when reg_write_source_ex_wb = reg_write_source_mem else
    received_word when reg_write_source_ex_wb = reg_write_source_rs  else
    program_counter_plus1_ex_wb & "00"
                  when reg_write_source_ex_wb = reg_write_source_pc  else
    fs_val        when reg_write_source_ex_wb = reg_write_source_fpr else
    (others => '-');

  -- TODO: make it combinational
  writeback_process: process(clk, rst)
  begin
    if rst = '1' then
      program_counter <= (others => '0');
    elsif rising_edge(clk) then
      if cpu_state = writeback then
        case branch_target is
        when branch_target_normal =>
          program_counter <= program_counter_plus1;
        when branch_target_j =>
          program_counter <= program_counter(29 downto 26) &
                             jump_immediate_val;
        when branch_target_b =>
          if TO_X01(branch_by_cop1) = 'X' then
            report "metavalue detected in branch_by_cop1" severity failure;
          elsif branch_by_cop1 = '1' then
            if TO_X01(fp_cond) = 'X' then
              report "metavalue detected in fp_cond" severity failure;
              program_counter <= (others => 'X');
            elsif TO_X01(branch_negate) = 'X' then
              report "metavalue detected in branch_negate" severity failure;
              program_counter <= (others => 'X');
            else
              if (fp_cond xor branch_negate) = '1' then
                program_counter <= program_counter_plus1_plusimm;
              else
                program_counter <= program_counter_plus1;
              end if;
            end if;
          else
            if TO_X01(alu_iszero) = 'X' then
              report "metavalue detected in alu_iszero" severity failure;
              program_counter <= (others => 'X');
            elsif TO_X01(branch_negate) = 'X' then
              report "metavalue detected in branch_negate" severity failure;
              program_counter <= (others => 'X');
            else
              if (alu_iszero xor branch_negate) = '1' then
                program_counter <= program_counter_plus1_plusimm;
              else
                program_counter <= program_counter_plus1;
              end if;
            end if;
          end if;
        when branch_target_alu =>
          -- TODO: alignment check
          program_counter <= alu_out(31 downto 2);
        when branch_target_dontcare =>
          program_counter <= (others => '-');
        end case;
      else
      end if;
      if cpu_state = writeback then
        fpr_we <= fp_reg_write;
        case fp_reg_write_source is
        when fp_reg_write_source_fs =>
          fd_val <= fs_val;
        when fp_reg_write_source_gpr =>
          fd_val <= rt_val;
        when fp_reg_write_source_fpu =>
          fd_val <= fpu_out;
        when fp_reg_write_source_dontcare =>
          fd_val <= (others => '-');
        end case;
        if TO_X01(fp_cond_write) = 'X' then
          report "metavalue detected in fp_cond_write" severity warning;
          fp_cond <= '-';
        elsif fp_cond_write = '1' then
          fp_cond <= fpu_condition;
        end if;
      else
        fpr_we <= '0';
        fd_val <= (others => '-');
      end if;
    end if;
  end process writeback_process;

  stall_writeback <= '0';

  fpu_controller: process(fpu_op, instruction_register)
  begin
    case fpu_op is
    when fpu_op_normal =>
      fpu_control <= instruction_register(5 downto 0);
    when fpu_op_dontcare =>
      fpu_control <= (others => '-');
    end case;
  end process fpu_controller;

  reg : register_file
  port map (
    clk => clk,
    rst => rst,
    gpr_rd0addr => rs_addr_id,
    gpr_rd0val => rs_val_id_ex,
    gpr_rd1addr => rt_addr_id,
    gpr_rd1val => rt_val_id_ex,
    gpr_wraddr => rd_addr_wb,
    gpr_wrval => rd_val_wb,
    gpr_we => gpr_we_wb);

  alu_unit : alu
  port map (
    alu_control => alu_control_id_ex,
    alu_in0 => alu_in0_ex,
    alu_in1 => alu_in1_ex,
    alu_out => alu_out_ex,
    alu_iszero => alu_iszero_ex);

  fp_reg : fp_register_file
  port map (
    clk => clk,
    rst => rst,
    fpr_rd0addr => fs_addr_id,
    fpr_rd0val => fs_val_id_ex,
    fpr_rd1addr => ft_addr_id,
    fpr_rd1val => ft_val_id_ex,
    fpr_wraddr => fd_addr_wb,
    fpr_wrval => fd_val_wb,
    fpr_we => fpr_we_wb);

  fpu_unit : fpucore
  port map (
    clk => clk,
    op => fpu_control_id_ex,
    in_1 => fpu_in0_ex,
    in_2 => fpu_in1_ex,
    out_1 => fpu_out_ex,
    cond => fpu_condition_ex);
end behavioral;
