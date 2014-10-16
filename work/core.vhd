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
    alu_control : out unsigned(3 downto 0);
    alu_in0 : out unsigned(31 downto 0);
    alu_in1 : out unsigned(31 downto 0);
    alu_out : in unsigned(31 downto 0);
    alu_iszero : in std_logic;
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);
end entity core;

architecture behavioral of core is
  signal cpu_state : cpu_state_t := program_loading;

  signal loading_word : unsigned(31 downto 0);
  subtype load_pos_t is integer range 0 to 4;
  signal load_pos : load_pos_t := 4;

  type memory_access_state_t is (
    memory_0, memory_1, memory_2, memory_3, memory_4);
  signal memory_access_state : memory_access_state_t;

  type instruction_memory_t is
    array(1023 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(29 downto 0) := (others => '0');

  signal instruction_register : unsigned(31 downto 0);

  signal opcode : unsigned(5 downto 0);
  signal funct : unsigned(5 downto 0);
  signal jump_immediate_val : unsigned(25 downto 0);
  signal immediate_val : unsigned(31 downto 0);

  signal rd_addr1 : unsigned(4 downto 0);
  signal rd_addr2 : unsigned(4 downto 0);

  signal alu_op : unsigned(1 downto 0);
  signal alu_src : std_logic;
  signal reg_dst : std_logic;
begin
  alu_in0 <= rs_val;
  alu_in1 <= (others => 'X') when TO_X01(alu_src) = 'X' else
             immediate_val when alu_src = '1' else rt_val;

  opcode <= instruction_register(31 downto 26);
  rs_addr <= instruction_register(25 downto 21);
  rt_addr <= instruction_register(20 downto 16);
  rd_addr <= (others => 'X') when TO_X01(reg_dst) = 'X' else
             rd_addr2 when reg_dst = '1' else rd_addr1;

  sequential : process(clk, rst)
    variable next_program_counter : unsigned(29 downto 0);
    variable next_rd_val : unsigned(31 downto 0);
    variable next_gpr_we : std_logic;
    variable next_cpu_state : cpu_state_t;

    variable next_mem_data_write : unsigned(31 downto 0);
    variable next_mem_we : std_logic;

    variable next_rs232c_recv_consume : std_logic;
    variable next_rs232c_send_bottom : unsigned(7 downto 0);
    variable next_rs232c_send_push : std_logic;

    variable imm_signext : std_logic;
  begin
    if rst = '1' then
      cpu_state <= program_loading;
      program_counter <= (others => '0');
      load_pos <= 4;
      rs232c_recv_consume <= '0';
      rs232c_send_push <= '0';
    elsif rising_edge(clk) then
      -- report "cpu_state = " & cpu_state_t'image(cpu_state);
      next_rd_val := (others => '-');
      next_gpr_we := '0';
      next_cpu_state := cpu_state;
      next_mem_data_write := (others => '-');
      next_mem_we := '0';
      next_rs232c_recv_consume := '0';
      next_rs232c_send_bottom := (others => '-');
      next_rs232c_send_push := '0';
      case cpu_state is
      when program_loading =>
        if load_pos = 0 then
          load_pos <= 4;
          if loading_word = (31 downto 0 => '1') then
            next_cpu_state := instruction_fetch;
            program_counter <= (others => '0');
          else
            instruction_memory(to_integer(program_counter(9 downto 0)))
              <= loading_word;
            program_counter <= program_counter + 1;
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
      when instruction_fetch =>
        assert TO_01(program_counter, 'X')(0) /= 'X'
          report "metavalue detected in program_counter"
            severity warning;
        instruction_register <=
          instruction_memory(to_integer(program_counter(9 downto 0)));
        next_cpu_state := decode;
      when decode =>
        assert TO_01(instruction_register, 'X')(0) /= 'X'
          report "metavalue detected in instruction_register; " &
                 "program_counter = " &
                 integer'image(to_integer(program_counter))
            severity warning;
        rd_addr1 <= instruction_register(20 downto 16);
        rd_addr2 <= instruction_register(15 downto 11);
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
        case opcode_t(to_integer(opcode)) is
        when OP_SPECIAL =>
          alu_op <= "10";
          alu_src <= '0';
        when OP_BEQ =>
          alu_op <= "01";
          alu_src <= '0';
        when OP_LW =>
          alu_op <= "00";
          alu_src <= '1';
        when OP_SW =>
          alu_op <= "00";
          alu_src <= '1';
        when others =>
        end case;
        next_cpu_state := execute;
      when execute =>
        next_cpu_state := writeback;
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_SPECIAL =>
        when OP_BEQ =>
        when OP_LW =>
          next_cpu_state := memory_access;
          memory_access_state <= memory_0;
          -- TODO memory alignment check
          mem_addr <= alu_out(31 downto 2);
        when OP_SW =>
          next_cpu_state := memory_access;
          memory_access_state <= memory_0;
          mem_addr <= alu_out(31 downto 2);
          next_mem_data_write := rt_val;
          next_mem_we := '1';
        when others =>
        end case;
      when memory_access =>
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_LW =>
          case memory_access_state is
          when memory_0 =>
            memory_access_state <= memory_1;
          when memory_1 =>
            next_cpu_state := writeback;
          when others =>
          end case;
        when OP_SW =>
          case memory_access_state is
          when memory_0 =>
            memory_access_state <= memory_1;
          when memory_1 =>
            next_cpu_state := writeback;
          when others =>
          end case;
        when others =>
          next_cpu_state := writeback;
        end case;
      when writeback =>
        next_program_counter := program_counter + 1;
        assert TO_01(opcode, 'X')(0) /= 'X'
          report "metavalue detected in opcode"
            severity warning;
        case opcode_t(to_integer(opcode)) is
        when OP_SPECIAL =>
          next_rd_val := alu_out;
          next_gpr_we := '1';
        when OP_J =>
          next_program_counter := program_counter(29 downto 26) &
                                  jump_immediate_val;
        when OP_BEQ =>
          if alu_iszero = '1' then
            next_program_counter :=
              program_counter + 1 + immediate_val(29 downto 0);
          end if;
        when OP_LW =>
          next_rd_val := mem_data_read;
          next_gpr_we := '1';
        when OP_SW =>
        when OP_RRB =>
          -- read word from RS-232C, blocking
          if rs232c_recv_empty /= '1' then
            next_rs232c_recv_consume := '1';
            next_rd_val := (31 downto 8 => '0') & rs232c_recv_top;
            next_gpr_we := '1';
          else
            next_program_counter := program_counter;
          end if;
        when OP_RSB =>
          -- send word into RS-232C, blocking
          if rs232c_send_full /= '1' then
            next_rs232c_send_bottom := rt_val(7 downto 0);
            next_rs232c_send_push := '1';
          else
            next_program_counter := program_counter;
          end if;
        when others =>
          report "unknown opcode " &
            integer'image(to_integer(opcode))
            severity warning;
        end case;
        next_cpu_state := instruction_fetch;
        program_counter <= next_program_counter;
      end case;
      rd_val <= next_rd_val;
      gpr_we <= next_gpr_we;
      cpu_state <= next_cpu_state;
      mem_data_write <= next_mem_data_write;
      mem_we <= next_mem_we;
      rs232c_recv_consume <= next_rs232c_recv_consume;
      rs232c_send_bottom <= next_rs232c_send_bottom;
      rs232c_send_push <= next_rs232c_send_push;
    end if;
  end process sequential;

  alu_controller: process(alu_op, immediate_val)
  begin
    case alu_op is
    when "00" =>
      alu_control <= "0010";
    when "01" =>
      alu_control <= "0110";
    when "10" =>
      case immediate_val(5 downto 0) is
      when "100001" =>
        alu_control <= "0010";
      when others =>
        alu_control <= (others => '-');
      end case;
    when others =>
      alu_control <= (others => '-');
    end case;
  end process alu_controller;
end behavioral;
