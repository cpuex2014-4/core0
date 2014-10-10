library ieee;
use ieee.std_logic_1164.all;

package serial is
  subtype databit_t is integer range 5 to 8;
  type parity_type is
    (parity_odd, parity_even, parity_mark, parity_space, parity_none);
  type handshaking_type is
    (handshaking_none, handshaking_software);
  -- baudrate
  -- 300, 1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400, 460800,
  -- 500000, 576000, 921600, 1000000, 1152000, 1500000, 2000000, 2500000,
  -- 3000000, 3500000, 4000000
  component rs232c is
    generic (
      clk_freq : real; -- ex. 66.66e6
      baudrate : real; -- ex. 9600.0
      stopbit : real; -- ex. 1.0
      databit : databit_t; -- ex. 8
      parity : parity_type;
      handshaking : handshaking_type);
    port (
      clk : in std_logic;
      txd : out std_logic;
      rxd : in std_logic;
      dsr : out std_logic;
      dtr : in std_logic := '-';
      rts : out std_logic;
      cts : in std_logic := '-';
      send_busy : out std_logic;
      send_go : in std_logic;
      send_data : in std_logic_vector(7 downto 0);
      recv_busy : in std_logic;
      recv_done : out std_logic;
      recv_data : out std_logic_vector(7 downto 0));
  end component rs232c;
end package serial;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.serial.all;

entity rs232c is
  generic (
    clk_freq : real;
    baudrate : real;
    stopbit : real;
    databit : databit_t;
    parity : parity_type;
    handshaking : handshaking_type);
  port (
    clk : in std_logic;
    txd : out std_logic;
    rxd : in std_logic;
    dsr : out std_logic;
    dtr : in std_logic := '-';
    rts : out std_logic;
    cts : in std_logic := '-';
    send_busy : out std_logic;
    send_go : in std_logic;
    send_data : in std_logic_vector(7 downto 0);
    recv_busy : in std_logic;
    recv_done : out std_logic;
    recv_data : out std_logic_vector(7 downto 0));
end entity rs232c;

architecture RTL of rs232c is
  constant bitcnt : integer := integer(round(clk_freq/baudrate));
  constant stopcnt : integer := integer(floor(clk_freq/baudrate*stopbit*0.99));
  constant startcnt : integer := integer(round(clk_freq/baudrate/6.0));
  constant counter_size : natural := natural(ceil(log2(real(1+bitcnt))))+1;

  constant bitcnt_u : unsigned := to_unsigned(bitcnt-1, counter_size);
  constant stopcnt_u : unsigned := to_unsigned(stopcnt-1, counter_size);
  constant startcnt_u : unsigned := to_unsigned(startcnt-1, counter_size);

  signal rxd_latched : std_logic;
  signal rxd_latched2 : std_logic;

  signal send_buf : std_logic_vector(databit+1 downto 0) := (others => '1');
  signal send_counter : unsigned(counter_size-1 downto 0);
  signal send_state : unsigned(3 downto 0) := "0000";

  signal recv_buf : std_logic_vector(databit downto 0);
  signal recv_counter : unsigned(counter_size-1 downto 0) := bitcnt_u;
  signal recv_bitcounter : unsigned(counter_size-1 downto 0);
  signal recv_state : unsigned(3 downto 0) := "0000";

  alias recv_data_part : std_logic_vector(databit-1 downto 0)
          is recv_data(databit-1 downto 0);

  function calc_databitp(i:integer) return integer is
  begin
    if parity = parity_none then
      return i;
    else
      return i + 1;
    end if;
  end function calc_databitp;
  function calc_parity(d:std_logic_vector) return std_logic is
    variable p : std_logic := '0';
  begin
    if parity = parity_odd or parity = parity_mark or parity = parity_none then
      p := '1';
    end if;
    if parity = parity_odd or parity = parity_even then
      for i in d'range loop
        p := p xor d(i);
      end loop;
    end if;
    return p;
  end function calc_parity;
begin
  send_busy <= send_go when send_state = 0 else '1';
  txd <= send_buf(0);
  comm : process(clk)
    variable recv_done_v : std_logic;
    variable recv_buf_v : std_logic_vector(databit downto 0);
  begin
    if rising_edge(clk) then
      rxd_latched2 <= rxd;
      rxd_latched <= rxd_latched2;
      if send_state = 0 then
        if send_go = '1' then
          send_buf <=
            calc_parity(send_data(databit-1 downto 0)) &
            send_data(databit-1 downto 0) & "0";
          send_state <= "0001";
          send_counter <= bitcnt_u;
        end if;
      elsif send_counter = 0 then
        send_buf <= "1" & send_buf(databit+1 downto 1);
        if send_state <= calc_databitp(databit) then
          send_state <= send_state + 1;
          send_counter <= bitcnt_u;
        elsif send_state = calc_databitp(databit)+1 then
          send_state <= send_state + 1;
          send_counter <= stopcnt_u;
        else
          send_state <= "0000";
        end if;
      else
        -- report integer'image(to_integer(send_counter));
        send_counter <= send_counter - 1;
      end if;

      recv_done_v := '0';
      if recv_state = 0 then
        if bitcnt-startcnt_u < recv_counter and recv_counter < bitcnt then
          if rxd_latched = '1' then
            recv_counter <= recv_counter + 1;
          else
            recv_counter <= recv_counter - 1;
          end if;
        else
          if recv_counter = 0 then
            recv_state <= "0001";
            recv_counter <= bitcnt_u - startcnt_u - 1;
            recv_bitcounter <= to_unsigned(0, recv_bitcounter'length);
          else
            recv_counter <= recv_counter - 1;
          end if;
        end if;
      elsif recv_counter = 0 then
        if recv_bitcounter < bitcnt/2 then
          recv_buf_v := "0" & recv_buf(databit downto 1);
        else
          recv_buf_v := "1" & recv_buf(databit downto 1);
        end if;
        recv_buf <= recv_buf_v;
        if recv_state <= calc_databitp(databit)-1 then
          recv_state <= recv_state + 1;
          recv_counter <= bitcnt_u;
          recv_bitcounter <= to_unsigned(0, recv_bitcounter'length);
        elsif recv_state = calc_databitp(databit) then
          recv_state <= recv_state + 1;
          recv_counter <= stopcnt_u;
          recv_done_v := '1';
          if parity = parity_none then
            recv_data_part <= recv_buf_v(databit downto 1);
          else
            recv_data_part <= recv_buf_v(databit-1 downto 0);
          end if;
        else
          recv_state <= "0000";
          recv_counter <= bitcnt_u;
        end if;
      else
        if rxd_latched = '1' then
          recv_bitcounter <= recv_bitcounter + 1;
        end if;
        recv_counter <= recv_counter - 1;
      end if;
      recv_done <= recv_done_v;
    end if;
  end process;
end architecture RTL;
