library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;

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
    XRST : in std_logic);

end cpu;

architecture behavioral of cpu is
  constant clk_freq : real := 66.666e6;
  signal send_busy : std_logic;
  signal send_go : std_logic;
  signal send_data : std_logic_vector(7 downto 0);
  signal recv_busy : std_logic;
  signal recv_done : std_logic;
  signal recv_data : std_logic_vector(7 downto 0);

  type cpu_state_t is (program_loading, running);
  signal cpu_state : cpu_state_t := program_loading;

  type instruction_memory_t is
    array(1023 downto 0) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;

  signal program_counter : unsigned(31 downto 0) := (others => '0');

  signal instruction_register : unsigned(31 downto 0);

  type gprs_t is array(31 downto 0) of unsigned(31 downto 0);
  signal gprs : gprs_t;

  type recv_fifo_t is array(0 to 1023) of std_logic_vector(7 downto 0);
  signal recv_fifo : recv_fifo_t;
  signal recv_fifo_start : unsigned(9 downto 0);
  signal recv_fifo_end : unsigned(9 downto 0);
  signal recv_fifo_topword : unsigned(31 downto 0);

  type send_fifo_t is array(0 to 1023) of std_logic_vector(7 downto 0);
  signal send_fifo : send_fifo_t;
  signal send_fifo_start : unsigned(9 downto 0);
  signal send_fifo_end : unsigned(9 downto 0);
begin
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

  recv_into_fifo : process(clk, XRST)
  begin
    if XRST = '0' then
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

  send_from_fifo : process(clk, XRST)
    variable send_go_v : std_logic;
  begin
    if XRST = '0' then
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

  instruction_register <=
    instruction_memory(to_integer(program_counter(11 downto 2)));

  foo : process(clk, XRST)
    variable next_program_counter : unsigned(31 downto 0);
    variable reg1val : unsigned(31 downto 0);
  begin
    if XRST = '0' then
      cpu_state <= program_loading;
      program_counter <= (others => '0');
      recv_fifo_start <= (others => '0');
      send_fifo_end <= (others => '0');
    elsif rising_edge(clk) and cpu_state = program_loading then
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
    elsif rising_edge(clk) then
      next_program_counter := program_counter + 4;
      case instruction_register(31 downto 26) is
      when "000010" =>
        next_program_counter := "000000" & instruction_register(25 downto 0);
      when "111111" =>
        next_program_counter := next_program_counter;
        case instruction_register(5 downto 0) is
        when "000000" =>
          -- report "a";
          -- read word from RS-232C, blocking
          if recv_fifo_end - recv_fifo_start >= 4 then
            gprs(to_integer(instruction_register(15 downto 11)))
              <= recv_fifo_topword;
            recv_fifo_start <= recv_fifo_start + 4;
          else
            next_program_counter := program_counter;
          end if;
        when "000001" =>
          -- report "b";
          -- write word into RS-232C, blocking
          if send_fifo_start - send_fifo_end - 1 >= 4 then
            reg1val := gprs(to_integer(instruction_register(15 downto 11)));
            send_fifo(to_integer(send_fifo_end+0))
              <= std_logic_vector(reg1val(31 downto 24));
            send_fifo(to_integer(send_fifo_end+1))
              <= std_logic_vector(reg1val(23 downto 16));
            send_fifo(to_integer(send_fifo_end+2))
              <= std_logic_vector(reg1val(15 downto 8));
            send_fifo(to_integer(send_fifo_end+3))
              <= std_logic_vector(reg1val(7 downto 0));
            send_fifo_end <= send_fifo_end + 4;
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
    end if;
  end process foo;
end behavioral;

