library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.sramsim.all;
use work.kakeudon.all;

entity cpu_tb is
end entity cpu_tb;

architecture behavioral of cpu_tb is
  constant clk_freq : real := 66.666e6;
  constant test_baudrate : real := 4000000.0;
  constant bypass_io : boolean := true;
  constant bypass_program_loading : boolean := true;
  constant test_stopbit : real := 1.0;
  signal simclk : std_logic;
  signal txd : std_logic := '1';
  signal txd1 : std_logic := '1';
  signal rxd : std_logic := '1';
  signal rxd1 : std_logic := '1';
  signal send_busy : std_logic;
  signal send_go : std_logic := '0';
  signal send_data : std_logic_vector(7 downto 0);
  signal recv_busy : std_logic := '0';
  signal recv_done : std_logic;
  signal recv_data : std_logic_vector(7 downto 0);

  type t_char_file is file of character;
  file read_file : t_char_file open read_mode is "in.dat";
  file write_file : t_char_file open write_mode is "out.dat";

  signal rst : std_logic := '0';

  signal ZA : std_logic_vector(19 downto 0);
  signal ZCLKMA : std_logic_vector(1 downto 0);
  signal XZBE : std_logic_vector(3 downto 0);
  signal XWA : std_logic;
  signal XE1 : std_logic;
  signal E2A : std_logic;
  signal XE3 : std_logic;
  signal XGA : std_logic;
  signal ADVA : std_logic;
  signal XZCKE : std_logic;
  signal ZD : std_logic_vector(31 downto 0);
  signal ZDP : std_logic_vector(3 downto 0);
  signal ZZA : std_logic;
  signal XFT : std_logic;
  signal XLBO : std_logic;
begin
  -- rxd <= transport rxd1 after 10 ns;
  rxd <= rxd1;
  txd <= txd1;

  non_bypass_io : if not bypass_io generate


  rdf : process(simclk)
    variable read_byte : integer;
    variable send_go_v : std_logic;
    variable ch : character;
    variable bypass_program_loading_memo : boolean
      := bypass_program_loading;
    variable ch0, ch1, ch2, ch3 : character;
  begin
    while bypass_program_loading_memo loop
      read(read_file, ch0);
      read(read_file, ch1);
      read(read_file, ch2);
      read(read_file, ch3);
      if to_unsigned(character'pos(ch0), 8) &
         to_unsigned(character'pos(ch1), 8) &
         to_unsigned(character'pos(ch2), 8) &
         to_unsigned(character'pos(ch3), 8) = x"FFFFFFFF" then
        bypass_program_loading_memo := false;
      end if;
    end loop;
    if rising_edge(simclk) then
      send_go_v := '0';
      if send_busy /= '1' and not endfile(read_file) then
        read(read_file, ch);
        read_byte := character'pos(ch);
        send_data <= std_logic_vector(to_unsigned(read_byte, 8));
        send_go_v := '1';
      end if;
      send_go <= send_go_v;
    end if;
  end process rdf;

  wrf : process(simclk)
    variable write_byte : integer;
    variable send_go_v : std_logic;
  begin
    if rising_edge(simclk) then
      if recv_done = '1' then
        write_byte := to_integer(unsigned(recv_data));
        write(write_file, character'val(write_byte));
      end if;
    end if;
  end process wrf;

  end generate non_bypass_io;

  rdwr : rs232c
  generic map (
    clk_freq => clk_freq,
    baudrate => test_baudrate,
    stopbit => test_stopbit,
    databit => 8,
    parity => parity_none,
    handshaking => handshaking_none)
  port map (
    clk => simclk,
    rxd => txd,
    txd => rxd1,
    send_busy => send_busy,
    send_go => send_go,
    send_data => send_data,
    recv_busy => recv_busy,
    recv_done => recv_done,
    recv_data => recv_data);

  cpu_unit : cpu
  generic map (
    debug_out => false,
    debug_out_commit => false,
    bypass_io => bypass_io,
    bypass_program_loading => bypass_program_loading,
    rs_baudrate => test_baudrate,
    rs_stopbit => test_stopbit)
  port map (
    clk => simclk,
    RS_TX => txd1,
    RS_RX => rxd,
    rst => rst,
    ZA => ZA,
    ZCLKMA => ZCLKMA,
    XZBE => XZBE,
    XWA => XWA,
    XE1 => XE1,
    E2A => E2A,
    XE3 => XE3,
    XGA => XGA,
    ADVA => ADVA,
    XZCKE => XZCKE,
    ZD => ZD,
    ZDP => ZDP,
    ZZA => ZZA,
    XFT => XFT,
    XLBO => XLBO);

  sram_unit0 : GS8160Z18
  -- generic map (report_read=>true, report_write=>true)
  port map (
    A => ZA,
    CK => ZCLKMA(0),
    XBA => XZBE(0),
    XBB => XZBE(1),
    XW => XWA,
    XE1 => XE1,
    E2 => E2A,
    XE3 => XE3,
    XG => XGA,
    ADV => ADVA,
    XCKE => XZCKE,
    DQA => ZD(7 downto 0),
    DQB => ZD(15 downto 8),
    DQPA => ZDP(0),
    DQPB => ZDP(1),
    ZZ => ZZA,
    XFT => XFT,
    XLBO => XLBO);

  sram_unit1 : GS8160Z18
  -- generic map (report_read=>true, report_write=>true)
  port map (
    A => ZA,
    CK => ZCLKMA(1),
    XBA => XZBE(2),
    XBB => XZBE(3),
    XW => XWA,
    XE1 => XE1,
    E2 => E2A,
    XE3 => XE3,
    XG => XGA,
    ADV => ADVA,
    XCKE => XZCKE,
    DQA => ZD(23 downto 16),
    DQB => ZD(31 downto 24),
    DQPA => ZDP(2),
    DQPB => ZDP(3),
    ZZ => ZZA,
    XFT => XFT,
    XLBO => XLBO);

  clockgen: process
  begin
    rst <= '1';
    wait for 1 ns;
    rst <= '0';
    wait for 1 ns;
    clockloop: loop
      simclk <= '0';
      wait for 0.5 sec / clk_freq;
      simclk <= '1';
      wait for 0.5 sec / clk_freq;
    end loop clockloop;
  end process;
end architecture behavioral;
