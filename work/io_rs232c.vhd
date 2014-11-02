library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity io_rs232c is
  port (
    clk : in std_logic;
    rst : in std_logic;
    RS_RX : in std_logic;
    RS_TX : out std_logic;
    recv_empty : out std_logic;
    recv_top : out unsigned(7 downto 0);
    recv_consume : in std_logic;
    send_full : out std_logic;
    send_bottom : in unsigned(7 downto 0);
    send_push : in std_logic);
end entity io_rs232c;

architecture behavioral of io_rs232c is
  type recv_fifo_t is array(0 to 1023) of unsigned(7 downto 0);
  signal recv_fifo : recv_fifo_t;
  attribute ram_style of recv_fifo : signal is "block";
  signal recv_fifo_start : unsigned(9 downto 0) := (others => '0');
  signal recv_fifo_end : unsigned(9 downto 0) := (others => '0');

  type send_fifo_t is array(0 to 1023) of unsigned(7 downto 0);
  signal send_fifo : send_fifo_t;
  attribute ram_style of send_fifo : signal is "block";
  signal send_fifo_start : unsigned(9 downto 0) := (others => '0');
  signal send_fifo_end : unsigned(9 downto 0) := (others => '0');

  signal uart_send_busy : std_logic;
  signal uart_send_go : std_logic := '0';
  signal uart_send_data : std_logic_vector(7 downto 0);
  signal uart_recv_busy : std_logic := '0';
  signal uart_recv_done : std_logic;
  signal uart_recv_data : std_logic_vector(7 downto 0);

begin
  recv_empty <= '1' when recv_fifo_end = recv_fifo_start else '0';
  send_full <= '1' when send_fifo_start - send_fifo_end - 1 <= 1 else '0';

  recv_into_fifo : process(clk, rst)
    variable next_recv_fifo_start : unsigned(9 downto 0);
    variable next_recv_fifo_end : unsigned(9 downto 0);
  begin
    if rst = '1' then
      recv_fifo_start <= (others => '0');
      recv_fifo_end <= (others => '0');
      recv_top <= (others => '-');
    elsif rising_edge(clk) then
      next_recv_fifo_end := recv_fifo_end;
      next_recv_fifo_start := recv_fifo_start;
      if uart_recv_done = '1' then
        if recv_fifo_end + 1 /= recv_fifo_start then
          recv_fifo(to_integer(recv_fifo_end)) <= unsigned(uart_recv_data);
          next_recv_fifo_end := recv_fifo_end + 1;
        else
          report "RS232C receive fifo overflow" severity warning;
        end if;
      end if;
      if recv_consume = '1' then
        next_recv_fifo_start := recv_fifo_start + 1;
      end if;
      recv_fifo_end <= next_recv_fifo_end;
      recv_fifo_start <= next_recv_fifo_start;

      recv_top <= recv_fifo(to_integer(next_recv_fifo_start));
    end if;
  end process recv_into_fifo;

  send_from_fifo : process(clk, rst)
    variable uart_send_go_v : std_logic;
  begin
    if rst = '1' then
      send_fifo_end <= (others => '0');
      send_fifo_start <= (others => '0');
      uart_send_go <= '0';
    elsif rising_edge(clk) then
      uart_send_go_v := '0';
      if send_fifo_end /= send_fifo_start then
        if uart_send_busy /= '1' then
          uart_send_go_v := '1';
          uart_send_data <=
            std_logic_vector(send_fifo(to_integer(send_fifo_start)));
          send_fifo_start <= send_fifo_start + 1;
        end if;
      end if;
      uart_send_go <= uart_send_go_v;
      if send_push = '1' then
        send_fifo(to_integer(send_fifo_end)) <= send_bottom;
        send_fifo_end <= send_fifo_end + 1;
      end if;
    end if;
  end process send_from_fifo;

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
    send_busy => uart_send_busy,
    send_go => uart_send_go,
    send_data => uart_send_data,
    recv_busy => uart_recv_busy,
    recv_done => uart_recv_done,
    recv_data => uart_recv_data);
end architecture behavioral;
