library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.core.all;

entity cpu is
  port (
    -- SRAM
    ZD : inout std_logic_vector(31 downto 0); -- SRAM Data
    ZDP : inout std_logic_vector(3 downto 0); -- SRAM Data, Parity
    ZA : out std_logic_vector(19 downto 0); -- SRAM Address
    XE1, E2A, XE3 : out std_logic; -- SRAM Chip Enables
    XZBE : out std_logic_vector(3 downto 0); -- SRAM Byte Enables
    XGA : out std_logic; -- SRAM Output Enable
    XWA : out std_logic; -- SRAM Write Enable
    XZCKE : out std_logic; -- SRAM Clock Enable
    ZCLKMA : out std_logic_vector(1 downto 0); -- SRAM Clock
    ADVA : out std_logic; -- SRAM Burst Mode / Negative Load Address
    XFT : out std_logic; -- SRAM Flow Through Mode
    XLBO : out std_logic; -- SRAM Linear Burst Order
    ZZA : out std_logic; -- SRAM Sleep Mode
    -- Serial I/O
    RS_TX : out std_logic; -- RS-232C, output
    RS_RX : in std_logic; -- RS-232C, input
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);

end cpu;

architecture behavioral of cpu is
  constant clk_freq : real := 66.666e6;
  signal send_busy : std_logic;
  signal send_go : std_logic;
  signal send_data : std_logic_vector(7 downto 0);
  signal recv_busy : std_logic;
  signal recv_done : std_logic;
  signal recv_data : std_logic_vector(7 downto 0);

  type cpu_state_t is (
    program_loading,
    instruction_fetch,
    decode,
    execute,
    memory_access,
    writeback);
  signal cpu_state : cpu_state_t := program_loading;

  type memory_access_state_t is (
    memory_0, memory_1, memory_2, memory_3, memory_4);
  signal memory_access_state : memory_access_state_t;

  signal mem_addr : unsigned(31 downto 0);
  signal mem_data_write : unsigned(31 downto 0);
  signal mem_data_read : unsigned(31 downto 0);
  signal mem_we : std_logic;

  type instruction_memory_t is
    array(1023 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(29 downto 0) := (others => '0');

  signal instruction_register : unsigned(31 downto 0);

  signal opcode : unsigned(5 downto 0);
  signal rs_addr : unsigned(4 downto 0);
  signal rt_addr : unsigned(4 downto 0);
  signal rd_addr : unsigned(4 downto 0);
  signal shamt : unsigned(4 downto 0);
  signal funct : unsigned(5 downto 0);
  signal immediate_val : unsigned(31 downto 0);

  signal rs_val : unsigned(31 downto 0);
  signal rt_val : unsigned(31 downto 0);
  signal rd_val : unsigned(31 downto 0);
  signal gpr_we : std_logic;

  type recv_fifo_t is array(0 to 1023) of std_logic_vector(7 downto 0);
  signal recv_fifo : recv_fifo_t;
  signal recv_fifo_start : unsigned(9 downto 0);
  signal recv_fifo_end : unsigned(9 downto 0);
  signal recv_fifo_topword : unsigned(31 downto 0);

  type send_fifo_t is array(0 to 1023) of std_logic_vector(7 downto 0);
  signal send_fifo : send_fifo_t;
  signal send_fifo_start : unsigned(9 downto 0);
  signal send_fifo_end : unsigned(9 downto 0);
  signal send_fifo_add : std_logic_vector(7 downto 0);
  signal send_fifo_we : std_logic;
begin
  reg : register_file
  port map (
    clk => clk,
    rst => rst,
    gpr_rd0addr => rs_addr,
    gpr_rd0val => rs_val,
    gpr_rd1addr => rt_addr,
    gpr_rd1val => rt_val,
    gpr_wraddr => rd_addr,
    gpr_wrval => rd_val,
    gpr_we => gpr_we);

  mem : memory_controller
  port map (
    clk => clk,
    addr => mem_addr,
    data_write => mem_data_write,
    data_read => mem_data_read,
    we => mem_we,
    ZD => ZD,
    ZDP => ZDP,
    ZA => ZA,
    XE1 => XE1,
    E2A => E2A,
    XE3 => XE3,
    XZBE => XZBE,
    XGA => XGA,
    XWA => XWA,
    XZCKE => XZCKE,
    ZCLKMA => ZCLKMA,
    ADVA => ADVA,
    XFT => XFT,
    XLBO => XLBO,
    ZZA => ZZA);

  uart : rs232c
  generic map (
    clk_freq => clk_freq,
    baudrate => 460800.0,
    stopbit => 1.0,
    databit => 8,
    parity => parity_none,
    handshaking => handshaking_none)
  port map (
    clk => clk,
    txd => RS_TX,
    rxd => RS_RX,
    send_busy => send_busy,
    send_go => send_go,
    send_data => send_data,
    recv_busy => recv_busy,
    recv_done => recv_done,
    recv_data => recv_data);

  -- big endian
  recv_fifo_topword <=
    unsigned(recv_fifo(to_integer(recv_fifo_start))) &
    unsigned(recv_fifo(to_integer(recv_fifo_start+1))) &
    unsigned(recv_fifo(to_integer(recv_fifo_start+2))) &
    unsigned(recv_fifo(to_integer(recv_fifo_start+3)));

  recv_into_fifo : process(clk, rst)
  begin
    if rst = '1' then
      recv_fifo_end <= (others => '0');
    elsif rising_edge(clk) then
      if recv_done = '1' then
        if recv_fifo_end + 1 /= recv_fifo_start then
          recv_fifo(to_integer(recv_fifo_end)) <= recv_data;
          recv_fifo_end <= recv_fifo_end + 1;
        end if;
      end if;
    end if;
  end process recv_into_fifo;

  send_from_fifo : process(clk, rst)
    variable send_go_v : std_logic;
  begin
    if rst = '1' then
      send_fifo_start <= (others => '0');
      send_go <= '0';
    elsif rising_edge(clk) then
      send_go_v := '0';
      if send_fifo_end /= send_fifo_start then
        if send_busy /= '1' then
          send_go_v := '1';
          send_data <= send_fifo(to_integer(send_fifo_start));
          send_fifo_start <= send_fifo_start + 1;
        end if;
      end if;
      send_go <= send_go_v;
    end if;
  end process send_from_fifo;

  cpu_sequential_process : process(clk, rst)
    variable next_program_counter : unsigned(29 downto 0);
    variable next_rd_val : unsigned(31 downto 0);
    variable next_gpr_we : std_logic;
    variable next_cpu_state : cpu_state_t;

    variable next_mem_addr : unsigned(31 downto 0);
    variable next_mem_data_write : unsigned(31 downto 0);
    variable next_mem_we : std_logic;
  begin
    if rst = '1' then
      cpu_state <= program_loading;
      program_counter <= (others => '0');
      recv_fifo_start <= (others => '0');
      send_fifo_end <= (others => '0');
    elsif rising_edge(clk) then
      next_rd_val := (others => '-');
      next_gpr_we := '0';
      next_cpu_state := cpu_state;
      next_mem_addr := (others => '-');
      next_mem_data_write := (others => '-');
      next_mem_we := '0';
      case cpu_state is
      when program_loading =>
        if recv_fifo_end - recv_fifo_start >= 4 then
          if recv_fifo_topword = (31 downto 0 => '1') then
            next_cpu_state := instruction_fetch;
            recv_fifo_start <= recv_fifo_start + 4;
            program_counter <= (others => '0');
          else
            instruction_memory(to_integer(program_counter(9 downto 0)))
              <= recv_fifo_topword;
            recv_fifo_start <= recv_fifo_start + 4;
            program_counter <= program_counter + 1;
          end if;
        end if;
      when instruction_fetch =>
        instruction_register <=
          instruction_memory(to_integer(program_counter(9 downto 0)));
        next_cpu_state := decode;
      when decode =>
        opcode <= instruction_register(31 downto 26);
        if instruction_register(31 downto 26) = "000000" then
          -- TODO: JR/JALR/SYSCALL/BREAK row
          rs_addr <= instruction_register(25 downto 21);
          rt_addr <= instruction_register(20 downto 16);
          rd_addr <= instruction_register(15 downto 11);
          shamt <= instruction_register(10 downto 6);
          funct <= instruction_register(5 downto 0);
          immediate_val <= (others => '-');
        -- elsif instruction_register(31 downto 26) = "000001" then
          -- TODO : REGIMM
        elsif instruction_register(31 downto 27) = "00001" then
          rs_addr <= (others => '-');
          rt_addr <= (others => '-');
          rd_addr <= (others => '-');
          shamt <= (others => '-');
          funct <= (others => '-');
          immediate_val <= (31 downto 27 => '0') &
                           instruction_register(26 downto 0);
        else
          rs_addr <= instruction_register(25 downto 21);
          rt_addr <= instruction_register(20 downto 16);
          rd_addr <= instruction_register(20 downto 16);
          shamt <= (others => '-');
          funct <= (others => '-');
          if instruction_register(31 downto 28) = "0011" then
            immediate_val <= (31 downto 16 => '0') &
                             instruction_register(15 downto 0);
          else
            immediate_val <= (31 downto 16 =>
                               instruction_register(15)) &
                             instruction_register(15 downto 0);
          end if;
        end if;
        next_cpu_state := execute;
      when execute =>
        next_cpu_state := writeback;
        case opcode_t(to_integer(opcode)) is
        when OP_LW =>
          next_cpu_state := memory_access;
          memory_access_state <= memory_0;
          next_mem_addr := rs_val + immediate_val;
          -- report "read_addr = " & integer'image(to_integer(next_mem_addr));
        when OP_SW =>
          next_cpu_state := memory_access;
          memory_access_state <= memory_0;
          next_mem_addr := rs_val + immediate_val;
          -- report "rs_addr = " & integer'image(to_integer(rs_addr));
          -- report "rs_val = " & integer'image(to_integer(rs_val));
          -- report "immediate_val = " & integer'image(to_integer(immediate_val));
          -- report "addr = " & integer'image(to_integer(next_mem_addr));
          -- report "data_write = " & integer'image(to_integer(rt_val));
          next_mem_data_write := rt_val;
          next_mem_we := '1';
        when others =>
        end case;
      when memory_access =>
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
        case opcode_t(to_integer(opcode)) is
        when OP_J =>
          next_program_counter := "0000" & instruction_register(25 downto 0);
        when OP_LW =>
          next_rd_val := mem_data_read;
          next_gpr_we := '1';
          -- report "memory_value = " &
          --   integer'image(to_integer(mem_data_read));
        when OP_SW =>
        when OP_IO =>
          case instruction_register(5 downto 0) is
          when "000000" =>
            -- read word from RS-232C, blocking
            if recv_fifo_end - recv_fifo_start >= 1 then
              recv_fifo_start <= recv_fifo_start + 1;
              next_rd_val :=
                (31 downto 8 => '0') &
                unsigned(recv_fifo(to_integer(recv_fifo_start)));
              next_gpr_we := '1';
            else
              next_program_counter := program_counter;
            end if;
          when "000001" =>
            -- write word into RS-232C, blocking
            if send_fifo_end + 1 /= send_fifo_start then
              send_fifo(to_integer(send_fifo_end)) <=
                std_logic_vector(rs_val(7 downto 0));
              send_fifo_end <= send_fifo_end + 1;
            else
              next_program_counter := program_counter;
            end if;
          when others =>
            report "unknown function code for 0b111111: " &
              integer'image(to_integer(instruction_register(5 downto 0)));
          end case;
        when others =>
          report "unknown opcode " &
            integer'image(to_integer(instruction_register(31 downto 26)));
        end case;
        next_cpu_state := instruction_fetch;
        program_counter <= next_program_counter;
      end case;
      rd_val <= next_rd_val;
      gpr_we <= next_gpr_we;
      cpu_state <= next_cpu_state;
      mem_addr <= next_mem_addr;
      mem_data_write <= next_mem_data_write;
      mem_we <= next_mem_we;
    end if;
  end process cpu_sequential_process;
end behavioral;

