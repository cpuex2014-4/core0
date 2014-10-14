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
    running,
    instruction_fetch,
    decode,
    execute,
    memory_access,
    writeback);
  signal cpu_state : cpu_state_t := program_loading;

  type instruction_memory_t is
    array(1023 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(31 downto 0) := (others => '0');

  signal instruction_register : unsigned(31 downto 0);

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
  subtype send_fifo_pending_count_t is integer range 0 to 4;
  signal send_fifo_pending_word : unsigned(31 downto 0);
  signal send_fifo_pending_count : send_fifo_pending_count_t;
begin
  reg : register_file
  port map (
    clk => clk,
    rst => rst,
    gpr_rd0addr => instruction_register(25 downto 21),
    gpr_rd0val => rs_val,
    gpr_rd1addr => instruction_register(20 downto 16),
    gpr_rd1val => rt_val,
    gpr_wraddr => instruction_register(15 downto 11),
    gpr_wrval => rd_val,
    gpr_we => gpr_we);

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

  send_fifo_writer : process(clk, rst)
  begin
    if rst = '1' then
      send_fifo_end <= (others => '0');
    elsif rising_edge(clk) then
      if send_fifo_we = '1' then
        send_fifo(to_integer(send_fifo_end)) <= send_fifo_add;
        send_fifo_end <= send_fifo_end + 1;
      end if;
    end if;
  end process send_fifo_writer;

  instruction_register <=
    instruction_memory(to_integer(program_counter(11 downto 2)));

  cpu_combinatorical_process :
  process(rst, cpu_state,
          recv_fifo_end, recv_fifo_start, recv_fifo_topword,
          send_fifo_end, send_fifo_start, send_fifo_add, send_fifo_we,
          send_fifo_pending_count, send_fifo_pending_word)
    variable next_send_fifo_add : std_logic_vector(7 downto 0);
    variable next_send_fifo_we : std_logic;
  begin
    next_send_fifo_add := (others => '-');
    next_send_fifo_we := '0';
    if rst = '1' then
    elsif cpu_state = running then
      if send_fifo_end + 1 /= send_fifo_start then
        case send_fifo_pending_count is
        when 0 =>
        when 1 =>
          next_send_fifo_add :=
            std_logic_vector(send_fifo_pending_word(7 downto 0));
          next_send_fifo_we := '1';
        when 2 =>
          next_send_fifo_add :=
            std_logic_vector(send_fifo_pending_word(15 downto 8));
          next_send_fifo_we := '1';
        when 3 =>
          next_send_fifo_add :=
            std_logic_vector(send_fifo_pending_word(23 downto 16));
          next_send_fifo_we := '1';
        when 4 =>
          next_send_fifo_add :=
            std_logic_vector(send_fifo_pending_word(31 downto 24));
          next_send_fifo_we := '1';
        end case;
      end if;
    end if;
    send_fifo_add <= next_send_fifo_add;
    send_fifo_we <= next_send_fifo_we;
  end process cpu_combinatorical_process;

  cpu_sequential_process : process(clk, rst)
    variable next_program_counter : unsigned(31 downto 0);
    variable next_rd_val : unsigned(31 downto 0);
    variable next_gpr_we : std_logic;
  begin
    if rst = '1' then
      cpu_state <= program_loading;
      program_counter <= (others => '0');
      recv_fifo_start <= (others => '0');
      send_fifo_pending_count <= 0;
    elsif rising_edge(clk) then
      next_rd_val := (others => '-');
      next_gpr_we := '0';
      if cpu_state = program_loading then
        if recv_fifo_end - recv_fifo_start >= 4 then
          if recv_fifo_topword = (31 downto 0 => '1') then
            cpu_state <= running;
            recv_fifo_start <= recv_fifo_start + 4;
            program_counter <= (others => '0');
          else
            instruction_memory(to_integer(program_counter(11 downto 2)))
              <= recv_fifo_topword;
            recv_fifo_start <= recv_fifo_start + 4;
            program_counter <= program_counter + 4;
          end if;
        end if;
      elsif cpu_state = running then
        next_program_counter := program_counter + 4;
        case instruction_register(31 downto 26) is
        when "000010" =>
          next_program_counter := "000000" & instruction_register(25 downto 0);
        when "111111" =>
          next_program_counter := next_program_counter;
          case instruction_register(5 downto 0) is
          when "000000" =>
            -- read word from RS-232C, blocking
            if recv_fifo_end - recv_fifo_start >= 4 then
              recv_fifo_start <= recv_fifo_start + 4;
              next_rd_val := recv_fifo_topword;
              next_gpr_we := '1';
            else
              next_program_counter := program_counter;
            end if;
          when "000001" =>
            -- write word into RS-232C, blocking
            if send_fifo_pending_count = 0 then
              send_fifo_pending_word <= rs_val;
              send_fifo_pending_count <= 4;
            else
              next_program_counter := program_counter;
            end if;
          when others =>
            report "unknown function code for 0b111111: " &
              integer'image(to_integer(instruction_register(5 downto 0)));
          end case;
          -- report "foo";
        when others =>
          report "unknown opcode " &
            integer'image(to_integer(instruction_register(31 downto 26)));
        end case;
        program_counter <= next_program_counter;
        if send_fifo_end + 1 /= send_fifo_start then
          case send_fifo_pending_count is
          when 0 =>
          when 1 =>
            send_fifo_pending_count <= 0;
          when 2 =>
            send_fifo_pending_count <= 1;
          when 3 =>
            send_fifo_pending_count <= 2;
          when 4 =>
            send_fifo_pending_count <= 3;
          end case;
        end if;
      end if;
      rd_val <= next_rd_val;
      gpr_we <= next_gpr_we;
    end if;
  end process cpu_sequential_process;

  XE1 <= '0';
  E2A <= '1';
  XE3 <= '0';
  XGA <= '0';
  XZCKE <= '0';
  ZCLKMA <= (others => clk);
  ADVA <= '0';
  XLBO <= '1';
  ZZA <= '0';
  XFT <= '1';
  XZBE <= (others => '0');
  -- ZA <= (others => '0');
  -- ZD <= (others => 'Z');
  -- ZDP <= (others => 'Z');
  XWA <= '1';
end behavioral;

