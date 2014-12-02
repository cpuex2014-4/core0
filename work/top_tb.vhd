library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.sramsim.all;
use work.kakeudon.all;

entity top_tb is
end entity top_tb;

architecture behavioral of top_tb is
  constant clk_freq : real := 66.666e6;
  constant test_baudrate : real := 460800.0;
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

  component top is
    port (
      -- DRAM
      DQ : inout std_logic_vector(63 downto 0); -- DRAM Data
      A : out std_logic_vector(13 downto 0); -- DRAM Address
      DQS, XDQS : inout std_logic_vector(7 downto 0); -- DRAM Data Strobes
      DM : out std_logic_vector(7 downto 0); -- DRAM Data Masks
      XCS : out std_logic_vector(1 downto 0); -- DRAM Chip Selects
      BA : out std_logic_vector(2 downto 0); -- DRAM Bank Address
      XRAS : out std_logic; -- DRAM Row Address Strobe
      XCAS : out std_logic; -- DRAM Column Address Strobe
      XWE : out std_logic; -- DRAM Write Enable
      ODT : out std_logic_vector(1 downto 0); -- DRAM On-Die Termination Control
      CKE : out std_logic_vector(1 downto 0); -- DRAM Clock Enables
      CK, XCK : out std_logic_vector(1 downto 0); -- DRAM Clock
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
      -- USB
      U_D : inout std_logic_vector(15 downto 0); -- USB Controller, FIFO Data Bus
      U_A : in std_logic_vector(11 downto 0); -- USB Controller, GPIF Output
      U_SP : in std_logic_vector(7 downto 0); -- USB Controller, output
      XU_CE : in std_logic; -- USB Controller, GPIF control output
      XU_RD : in std_logic; -- USB Controller, GPIF control output
      XU_WE : in std_logic; -- USB Controller, GPIF control output
      U_CTL : in std_logic_vector(5 downto 3); -- USB Controller, GPIF control output
      U_RDY : out std_logic_vector(5 downto 1); -- USB Controller
      XU_RYBY : out std_logic; -- USB Controller
      U_INT4 : out std_logic; -- USB Controller
      XU_INT5 : out std_logic; -- USB Controller
      -- Universal-I/O Connection
      IOA : inout std_logic_vector(126 downto 1);
      -- Serial I/O
      RS_TX : out std_logic; -- RS-232C, output
      USB_RSRX : out std_logic; -- USB, output
      RS_RX : in std_logic; -- RS-232C, input
      USB_RSTX : in std_logic; -- USB, input
      -- Clock
      DUMY : out std_logic; -- dummy?
      CLK48M : in std_logic; -- 48MHz clock for USB
      MCLK1 : in std_logic; -- 66.666MHz master clock
      XRST : in std_logic; -- Reset Signal?
      -- PCI Express, SMBus
      MPERST : out std_logic; -- reset?
      MSMCLK : inout std_logic; -- I2C Clock
      MSMDAT : inout std_logic; -- I2C Data
      MWAKE : out std_logic); -- Wakeup?

  end component top;

  signal XRST : std_logic := '1';

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

      -- DQ : inout std_logic_vector(63 downto 0); -- DRAM Data
      -- DQS, XDQS : inout std_logic_vector(7 downto 0); -- DRAM Data Strobes
      -- U_D : inout std_logic_vector(15 downto 0); -- USB Controller, FIFO Data Bus
      -- IOA : inout std_logic_vector(126 downto 1);
      -- MSMCLK : inout std_logic; -- I2C Clock
      -- MSMDAT : inout std_logic; -- I2C Data
      -- USB
  signal U_A : std_logic_vector(11 downto 0);
  signal U_SP : std_logic_vector(7 downto 0);
  signal XU_CE : std_logic;
  signal XU_RD : std_logic;
  signal XU_WE : std_logic;
  signal U_CTL : std_logic_vector(5 downto 3);
  signal RS_RX : std_logic;
  signal USB_RSTX : std_logic;
  signal CLK48M : std_logic;
begin
  -- rxd <= transport rxd1 after 10 ns;
  rxd <= rxd1;
  txd <= txd1;
  rdf : process(simclk)
    variable read_byte : integer;
    variable send_go_v : std_logic;
    variable ch : character;
  begin
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

  top_unit : top
  port map (
    -- DRAM
    DQ => open,
    A => open,
    DQS => open, XDQS => open,
    DM => open,
    XCS => open,
    BA => open,
    XRAS => open,
    XCAS => open,
    XWE => open,
    ODT => open,
    CKE => open,
    CK => open, XCK => open,
    -- SRAM
    ZD => ZD,
    ZDP => ZDP,
    ZA => ZA,
    XE1 => XE1, E2A => E2A, XE3 => XE3,
    XZBE => XZBE,
    XGA => XGA,
    XWA => XWA,
    XZCKE => XZCKE,
    ZCLKMA => ZCLKMA,
    ADVA => ADVA,
    XFT => XFT,
    XLBO => XLBO,
    ZZA => ZZA,
    -- USB
    U_D => open,
    U_A => U_A,
    U_SP => U_SP,
    XU_CE => XU_CE,
    XU_RD => XU_RD,
    XU_WE => XU_WE,
    U_CTL => U_CTL,
    U_RDY => open,
    XU_RYBY => open,
    U_INT4 => open,
    XU_INT5 => open,
    -- Universal-I/O Connection
    IOA => open,
    -- Serial I/O
    RS_TX => txd,
    USB_RSRX => open,
    RS_RX => rxd,
    USB_RSTX => USB_RSTX,
    DUMY => open,
    CLK48M => CLK48M,
    MCLK1 => simclk,
    XRST => XRST,
    MPERST => open,
    MSMCLK => open,
    MSMDAT => open,
    MWAKE => open);

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
    XRST <= '0';
    wait for 1 ns;
    XRST <= '1';
    wait for 1 ns;
    clockloop: loop
      simclk <= '0';
      wait for 0.5 sec / clk_freq;
      simclk <= '1';
      wait for 0.5 sec / clk_freq;
    end loop clockloop;
  end process;
end architecture behavioral;
