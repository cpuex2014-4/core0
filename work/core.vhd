library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity core is
  port (
    -- Register File
    rs_addr : out unsigned(4 downto 0);
    rs_val : in unsigned(31 downto 0);
    rt_addr : out unsigned(4 downto 0);
    rt_val : in unsigned(31 downto 0);
    rd_addr : out unsigned(4 downto 0);
    rd_val : out unsigned(31 downto 0);
    gpr_we : out std_logic;
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
    -- ALU
    alu_control : out unsigned(4 downto 0);
    alu_in0 : out unsigned(31 downto 0);
    alu_in1 : out unsigned(31 downto 0);
    alu_out : in unsigned(31 downto 0);
    alu_iszero : in std_logic;
    -- Floating-Point Register File
    fs_addr : out unsigned(4 downto 0);
    fs_val : in unsigned(31 downto 0);
    ft_addr : out unsigned(4 downto 0);
    ft_val : in unsigned(31 downto 0);
    fd_addr : out unsigned(4 downto 0);
    fd_val : out unsigned(31 downto 0);
    fpr_we : out std_logic;
    -- FPU
    fpu_control : out unsigned(5 downto 0);
    fpu_in0 : out unsigned(31 downto 0);
    fpu_in1 : out unsigned(31 downto 0);
    fpu_out : in unsigned(31 downto 0);
    fpu_condition : in std_logic;
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);
end entity core;

architecture behavioral of core is
  type cpu_state_t is (
    program_loading,
    instruction_fetch,
    decode,
    execute,
    memory_access_0,
    memory_access_1,
    writeback);
  signal cpu_state : cpu_state_t := program_loading;

  signal loading_counter : unsigned(29 downto 0) := (others => '0');
  signal loading_word : unsigned(31 downto 0);
  subtype load_pos_t is integer range 0 to 4;
  signal load_pos : load_pos_t := 4;

  signal received_word : unsigned(31 downto 0);

  type instruction_memory_t is
    array(1023 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(29 downto 0) := (others => '0');
  signal program_counter_plus1 : unsigned(29 downto 0);
  signal program_counter_plus1_plusimm : unsigned(29 downto 0);

  signal instruction_register : unsigned(31 downto 0);

  signal opcode : unsigned(5 downto 0);
  signal funct : unsigned(5 downto 0);
  signal jump_immediate_val : unsigned(25 downto 0);
  signal immediate_val : unsigned(31 downto 0);

  signal rd_addr1 : unsigned(4 downto 0);
  signal rd_addr2 : unsigned(4 downto 0);

  type rs_io_mode_t is (rs_io_normal, rs_io_recv, rs_io_send);
  signal rs_io_mode : rs_io_mode_t;
  type branch_target_t is (branch_target_normal,
                           branch_target_j, branch_target_b,
                           branch_target_alu,
                           branch_target_dontcare);
  signal branch_target : branch_target_t;
  type alu_op_t is (alu_op_add, alu_op_sub, alu_op_normal, alu_op_dontcare);
  signal alu_op : alu_op_t;
  signal alu_src_a : std_logic;
  signal alu_src_b : std_logic;
  signal reg_dst : std_logic;
  signal reg_write : std_logic;
  type reg_write_source_t is (
    reg_write_source_alu,
    reg_write_source_mem,
    reg_write_source_rs,
    reg_write_source_pc,
    reg_write_source_fpr,
    reg_write_source_dontcare);
  signal reg_write_source : reg_write_source_t;

  type fpu_op_t is (fpu_op_normal, fpu_op_dontcare);
  signal fpu_op : fpu_op_t;
  signal fd_addr1 : unsigned(4 downto 0);
  signal fd_addr2 : unsigned(4 downto 0);
  signal fp_reg_dst : std_logic;
  signal fp_reg_write : std_logic;
  type fp_reg_write_source_t is (
    fp_reg_write_source_fs,
    fp_reg_write_source_gpr,
    fp_reg_write_source_fpu,
    fp_reg_write_source_dontcare);
  signal fp_reg_write_source : fp_reg_write_source_t;
begin
  alu_in0 <= (others => 'X') when TO_X01(alu_src_a) = 'X' else
             rs_val when alu_src_a = '0' else
             (31 downto 5 => '0') & immediate_val(10 downto 6);
  alu_in1 <= (others => 'X') when TO_X01(alu_src_b) = 'X' else
             rt_val when alu_src_b = '0' else
             immediate_val;

  opcode <= instruction_register(31 downto 26);
  rs_addr <= instruction_register(25 downto 21);
  rt_addr <= instruction_register(20 downto 16);
  rd_addr <= (others => '-') when
                reg_write_source = reg_write_source_dontcare else
             (others => '1') when reg_write_source = reg_write_source_pc else
             (others => 'X') when TO_X01(reg_dst) = 'X' else
             rd_addr2 when reg_dst = '1' else rd_addr1;

  fpu_in0 <= ft_val;
  fpu_in1 <= fs_val;

  fs_addr <= instruction_register(15 downto 11);
  ft_addr <= instruction_register(20 downto 16);
  fd_addr <= (others => 'X') when TO_X01(fp_reg_dst) = 'X' else
             fd_addr2 when fp_reg_dst = '1' else fd_addr1;

  sequential : process(clk, rst)
    variable next_cpu_state : cpu_state_t;
  begin
    if rst = '1' then
      cpu_state <= program_loading;
    elsif rising_edge(clk) then
      -- assert cpu_state = program_loading or cpu_state = memory_access_0
      --   report "cpu_state = " & cpu_state_t'image(cpu_state);
      next_cpu_state := cpu_state;
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
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_LW =>
          next_cpu_state := memory_access_0;
        when OP_SW =>
          next_cpu_state := memory_access_0;
        when OP_RRB =>
          next_cpu_state := memory_access_0;
        when OP_RSB =>
          next_cpu_state := memory_access_0;
        when others =>
          next_cpu_state := writeback;
        end case;
      when memory_access_0 =>
        case rs_io_mode is
        when rs_io_normal =>
          next_cpu_state := memory_access_1;
        when rs_io_recv =>
          -- read word from RS-232C, blocking
          if rs232c_recv_empty /= '1' then
            next_cpu_state := memory_access_1;
          end if;
        when rs_io_send =>
          -- send word into RS-232C, blocking
          if rs232c_send_full /= '1' then
            next_cpu_state := memory_access_1;
          end if;
        end case;
      when memory_access_1 =>
        next_cpu_state := writeback;
      when writeback =>
        next_cpu_state := instruction_fetch;
      end case;
      cpu_state <= next_cpu_state after 1 ns;
    end if;
  end process sequential;

  instruction_fetch_process: process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if cpu_state = instruction_fetch then
        assert TO_01(program_counter, 'X')(0) /= 'X'
          report "metavalue detected in program_counter"
            severity warning;
        instruction_register <=
          instruction_memory(to_integer(program_counter(9 downto 0)));
        program_counter_plus1 <=
          program_counter + 1;
      end if;
    end if;
  end process instruction_fetch_process;

  decode_process: process(clk, rst)
    variable imm_signext : std_logic;
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if cpu_state = decode then
        assert TO_01(instruction_register, 'X')(0) /= 'X'
          report "metavalue detected in instruction_register; " &
                 "program_counter = " &
                 integer'image(to_integer(program_counter))
            severity warning;
        rd_addr1 <= instruction_register(20 downto 16);
        rd_addr2 <= instruction_register(15 downto 11);
        fd_addr1 <= instruction_register(10 downto 6);
        fd_addr2 <= instruction_register(15 downto 11);
        jump_immediate_val <= instruction_register(25 downto 0);
        if instruction_register(31 downto 26) = "000000" then
          -- TODO: JR/JALR/SYSCALL/BREAK row
          reg_dst <= '1';
          imm_signext := '-';
        -- elsif instruction_register(31 downto 26) = "000001" then
          -- TODO : REGIMM
        elsif instruction_register(31 downto 27) = "00001" then
          reg_dst <= '-';
          imm_signext := '-';
        else
          reg_dst <= '0';
          if instruction_register(31 downto 28) = "0011" then
            imm_signext := '0';
          else
            imm_signext := '1';
          end if;
        end if;
        immediate_val(15 downto 0) <= instruction_register(15 downto 0);
        if TO_X01(imm_signext) = 'X' then
          immediate_val(31 downto 16) <= (others => 'X');
        elsif imm_signext = '1' then
          immediate_val(31 downto 16) <= (others => instruction_register(15));
        else
          immediate_val(31 downto 16) <= (others => '0');
        end if;
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_SPECIAL =>
          if instruction_register(5 downto 3) = "001" then
            alu_op <= alu_op_add;
            alu_src_a <= '0';
            alu_src_b <= '0';
            reg_write <= '0';
            reg_write_source <= reg_write_source_dontcare;
            fpu_op <= fpu_op_dontcare;
            fp_reg_write <= '0';
            fp_reg_dst <= '-';
            fp_reg_write_source <= fp_reg_write_source_dontcare;
            rs_io_mode <= rs_io_normal;
            branch_target <= branch_target_alu;
          else
            alu_op <= alu_op_normal;
            if instruction_register(5 downto 2) = "0000" then
              alu_src_a <= '1';
            else
              alu_src_a <= '0';
            end if;
            alu_src_b <= '0';
            reg_write <= '1';
            reg_write_source <= reg_write_source_alu;
            fpu_op <= fpu_op_dontcare;
            fp_reg_write <= '0';
            fp_reg_dst <= '-';
            fp_reg_write_source <= fp_reg_write_source_dontcare;
            rs_io_mode <= rs_io_normal;
            branch_target <= branch_target_normal;
          end if;
        when OP_J =>
          alu_op <= alu_op_dontcare;
          alu_src_a <= '-';
          alu_src_b <= '-';
          reg_write <= '0';
          reg_write_source <= reg_write_source_dontcare;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_j;
        when OP_JAL =>
          alu_op <= alu_op_dontcare;
          alu_src_a <= '-';
          alu_src_b <= '-';
          reg_write <= '1';
          reg_write_source <= reg_write_source_pc;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_j;
        when OP_BEQ =>
          alu_op <= alu_op_sub;
          alu_src_a <= '0';
          alu_src_b <= '0';
          reg_write <= '0';
          reg_write_source <= reg_write_source_dontcare;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_b;
        when OP_ADDIU =>
          alu_op <= alu_op_normal;
          alu_src_a <= '0';
          alu_src_b <= '1';
          reg_write <= '1';
          reg_write_source <= reg_write_source_alu;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_normal;
        when OP_COP1 =>
          case instruction_register(25 downto 21) is
          when "00000" => -- MFC1
            alu_op <= alu_op_dontcare;
            alu_src_a <= '-';
            alu_src_b <= '-';
            reg_write <= '1';
            reg_write_source <= reg_write_source_fpr;
            fpu_op <= fpu_op_dontcare;
            fp_reg_write <= '0';
            fp_reg_dst <= '-';
            fp_reg_write_source <= fp_reg_write_source_dontcare;
            rs_io_mode <= rs_io_normal;
            branch_target <= branch_target_normal;
          when "00100" => -- MTC1
            alu_op <= alu_op_dontcare;
            alu_src_a <= '-';
            alu_src_b <= '-';
            reg_write <= '0';
            reg_write_source <= reg_write_source_dontcare;
            fpu_op <= fpu_op_dontcare;
            fp_reg_write <= '1';
            fp_reg_dst <= '1';
            fp_reg_write_source <= fp_reg_write_source_gpr;
            rs_io_mode <= rs_io_normal;
            branch_target <= branch_target_normal;
          when "10000" => -- Single-precision
            case instruction_register(5 downto 0) is
            when "000000" => -- add.s
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_normal;
              fp_reg_write <= '1';
              fp_reg_dst <= '0';
              fp_reg_write_source <= fp_reg_write_source_fpu;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_normal;
            when "000110" => -- mov.s
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_dontcare;
              fp_reg_write <= '1';
              fp_reg_dst <= '0';
              fp_reg_write_source <= fp_reg_write_source_fs;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_normal;
            when "100100" => -- cvt.w.s
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_normal;
              fp_reg_write <= '1';
              fp_reg_dst <= '0';
              fp_reg_write_source <= fp_reg_write_source_fpu;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_normal;
            when others =>
              report "fp_inst.s : unknown funct " &
                integer'image(to_integer(instruction_register(5 downto 0)))
                severity warning;
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_dontcare;
              fp_reg_write <= '-';
              fp_reg_dst <= '-';
              fp_reg_write_source <= fp_reg_write_source_dontcare;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_dontcare;
            end case;
          when "10100" => -- Fixed-point Word
            case instruction_register(5 downto 0) is
            when "100000" => -- cvt.s.w
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_normal;
              fp_reg_write <= '1';
              fp_reg_dst <= '0';
              fp_reg_write_source <= fp_reg_write_source_fpu;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_normal;
            when others =>
              report "fp_inst.s : unknown funct " &
                integer'image(to_integer(instruction_register(5 downto 0)))
                severity warning;
              alu_op <= alu_op_dontcare;
              alu_src_a <= '-';
              alu_src_b <= '-';
              reg_write <= '-';
              reg_write_source <= reg_write_source_dontcare;
              fpu_op <= fpu_op_dontcare;
              fp_reg_write <= '-';
              fp_reg_dst <= '-';
              fp_reg_write_source <= fp_reg_write_source_dontcare;
              rs_io_mode <= rs_io_normal;
              branch_target <= branch_target_dontcare;
            end case;
          when others =>
            report "COP1 : unknown fmt " &
              integer'image(to_integer(instruction_register(25 downto 21)))
              severity warning;
            alu_op <= alu_op_dontcare;
            alu_src_a <= '-';
            alu_src_b <= '-';
            reg_write <= '-';
            reg_write_source <= reg_write_source_dontcare;
            fpu_op <= fpu_op_dontcare;
            fp_reg_write <= '-';
            fp_reg_dst <= '-';
            fp_reg_write_source <= fp_reg_write_source_dontcare;
            rs_io_mode <= rs_io_normal;
            branch_target <= branch_target_dontcare;
          end case;
        when OP_LW =>
          alu_op <= alu_op_add;
          alu_src_a <= '0';
          alu_src_b <= '1';
          reg_write <= '1';
          reg_write_source <= reg_write_source_mem;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_normal;
        when OP_SW =>
          alu_op <= alu_op_add;
          alu_src_a <= '0';
          alu_src_b <= '1';
          reg_write <= '0';
          reg_write_source <= reg_write_source_dontcare;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_normal;
        when OP_RRB =>
          alu_op <= alu_op_add;
          alu_src_a <= '0';
          alu_src_b <= '0';
          reg_write <= '1';
          reg_write_source <= reg_write_source_rs;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_recv;
          branch_target <= branch_target_normal;
        when OP_RSB =>
          alu_op <= alu_op_add;
          alu_src_a <= '0';
          alu_src_b <= '0';
          reg_write <= '0';
          reg_write_source <= reg_write_source_dontcare;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '0';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_send;
          branch_target <= branch_target_normal;
        when others =>
          report "unknown opcode " &
            integer'image(to_integer(opcode))
            severity warning;
          alu_op <= alu_op_dontcare;
          alu_src_a <= '-';
          alu_src_b <= '-';
          reg_write <= '-';
          reg_write_source <= reg_write_source_dontcare;
          fpu_op <= fpu_op_dontcare;
          fp_reg_write <= '-';
          fp_reg_dst <= '-';
          fp_reg_write_source <= fp_reg_write_source_dontcare;
          rs_io_mode <= rs_io_normal;
          branch_target <= branch_target_dontcare;
        end case;
      end if;
    end if;
  end process decode_process;

  execute_process: process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if cpu_state = execute then
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_LW =>
          -- TODO memory alignment check
          mem_data_write <= (others => '-');
          mem_we <= '0';
        when OP_SW =>
          mem_data_write <= rt_val;
          mem_we <= '1';
        when others =>
        end case;
        mem_addr <= alu_out(31 downto 2);
        program_counter_plus1_plusimm <=
          program_counter_plus1 + immediate_val(29 downto 0);
      else
        mem_data_write <= (others => '-');
        mem_we <= '0';
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
            instruction_memory(to_integer(loading_counter(9 downto 0)))
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

  -- TODO: make it combinational
  writeback_process: process(clk, rst)
  begin
    if rst = '1' then
      program_counter <= (others => '0');
    elsif rising_edge(clk) then
      if cpu_state = writeback then
        gpr_we <= reg_write;
        case reg_write_source is
        when reg_write_source_alu =>
          rd_val <= alu_out;
        when reg_write_source_mem =>
          rd_val <= mem_data_read;
        when reg_write_source_rs =>
          rd_val <= received_word;
        when reg_write_source_pc =>
          rd_val <= program_counter_plus1 & "00";
        when reg_write_source_fpr =>
          rd_val <= fs_val;
        when reg_write_source_dontcare =>
          rd_val <= (others => '-');
        end case;
        case branch_target is
        when branch_target_normal =>
          program_counter <= program_counter_plus1;
        when branch_target_j =>
          program_counter <= program_counter(29 downto 26) &
                             jump_immediate_val;
        when branch_target_b =>
          if TO_X01(alu_iszero) = 'X' then
            report "metavalue detected in alu_iszero" severity failure;
            program_counter <= (others => 'X');
          elsif alu_iszero = '1' then
            program_counter <= program_counter_plus1_plusimm;
          else
            program_counter <= program_counter_plus1;
          end if;
        when branch_target_alu =>
          -- TODO: alignment check
          program_counter <= alu_out(31 downto 2);
        when branch_target_dontcare =>
          program_counter <= (others => '-');
        end case;
      else
        gpr_we <= '0';
        rd_val <= (others => '-');
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
      else
        fpr_we <= '0';
        fd_val <= (others => '-');
      end if;
    end if;
  end process writeback_process;

  alu_controller: process(alu_op, alu_src_a, alu_src_b, opcode, immediate_val)
  begin
    case alu_op is
    when alu_op_add =>
      alu_control <= "00010";
    when alu_op_sub =>
      alu_control <= "00110";
    when alu_op_normal =>
      if TO_X01(alu_src_b) = 'X' then
        alu_control <= (others => '-');
      elsif alu_src_b = '1' then
        case opcode_t(to_integer(opcode)) is
        when OP_ADDIU =>
          alu_control <= "00010";
        when others =>
          alu_control <= (others => '-');
        end case;
      else
        case immediate_val(5 downto 0) is
        when "000000" => -- SLL
          alu_control <= "10000";
        when "000010" => -- SRL
          alu_control <= "10010";
        when "000011" => -- SRA
          alu_control <= "10011";
        when "100000" => -- ADD
          -- TODO error handling
          alu_control <= "00010";
        when "100001" => -- ADDU
          alu_control <= "00010";
        when "100010" => -- SUB
          -- TODO error handling
          alu_control <= "00110";
        when "100011" => -- SUBU
          alu_control <= "00110";
        when "100100" => -- AND
          alu_control <= "00000";
        when "100101" => -- OR
          alu_control <= "00001";
        when "100110" => -- XOR
          alu_control <= "11100";
        when "100111" => -- NOR
          alu_control <= "01100";
        when "101010" => -- SLT
          alu_control <= "00111";
        when "101011" => -- SLTU
          alu_control <= "10111";
        when others =>
          alu_control <= (others => '-');
        end case;
      end if;
    when alu_op_dontcare =>
      alu_control <= (others => '-');
    end case;
  end process alu_controller;

  fpu_controller: process(fpu_op)
  begin
    case fpu_op is
    when fpu_op_normal =>
      fpu_control <= instruction_register(5 downto 0);
    when fpu_op_dontcare =>
      fpu_control <= (others => '-');
    end case;
  end process fpu_controller;
end behavioral;
