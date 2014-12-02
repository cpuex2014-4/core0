library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity top is
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

end top;

architecture toplevel of top is
  constant mclk_freq : real := 66.666e6;
  constant clk_mult : integer := 2;
  constant clk_div : integer := 1;
  constant clk_freq : real := 66.666e6 * real(clk_mult) / real(clk_div);
  signal imclk : std_logic;
  signal clk0, clk0bgi : std_logic;
  signal clk, clkbgi : std_logic;
  signal clk_plain : std_logic;

  signal dqsbuf : std_logic_vector(7 downto 0);
  signal ckbuf : std_logic_vector(1 downto 0);
  signal rst : std_logic;
begin
  cpu_unit: cpu
  generic map (
    debug_out => false,
    debug_out_commit => false,
    rs_baudrate => 460800.0,
    rs_stopbit => 1.0)
  port map (
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
    ZZA => ZZA,
    RS_TX => RS_TX,
    RS_RX => RS_RX,
    -- clk => clk,
    clk => clk_plain,
    rst => rst);
  ib: IBUFG
  port map (
    i => MCLK1,
    o => imclk);
  rst <= not XRST;
  dcm1 : DCM_ADV
  generic map (
    clkin_period => 1.0e9 / mclk_freq,
    clkfx_divide => clk_div,
    clkfx_multiply => clk_mult,
    dcm_autocalibration => false,
    clkout_phase_shift => "FIXED",
    phase_shift => 0,
    sim_device => "VIRTEX5")
  port map (
    clk0 => clk0bgi,
    clkfx => clkbgi,
    clkin => imclk,
    clkfb => clk0,
    rst => not XRST);
  bg0 : BUFG
  port map (
    i => clk0bgi,
    o => clk0);
  bg1 : BUFG
  port map (
    i => clkbgi,
    o => clk);
  bg2 : BUFG
  port map (
    i => imclk,
    o => clk_plain);

  ckouts : for cki in 0 to 1 generate
    ckout : OBUFDS
    port map (
      i => ckbuf(cki),
      o => CK(cki),
      ob => XCK(cki));
  end generate;

  dqs_outs : for dqsi in 0 to 7 generate
    dqs_out : OBUFDS
    port map (
      i => dqsbuf(dqsi),
      o => DQS(dqsi),
      ob => XDQS(dqsi));
  end generate;

  DQ <= (others => 'Z');
  A <= (others => '0');
  dqsbuf <= (others => '0');
  DM <= (others => '1');
  XCS <= (others => '0');
  BA <= (others => '0');
  XRAS <= '1';
  XCAS <= '1';
  XWE <= '1';
  ODT <= (others => '0');
  CKE <= (others => '0');
  ckbuf <= (others => '0'); -- clock off

  U_RDY <= (others => '0');
  XU_RYBY <= '1';
  U_INT4 <= '1';
  XU_INT5 <= '0';

  IOA <= (others => 'Z');

  USB_RSRX <= '1';

  DUMY <= 'Z';

  MPERST <= '0';
  MSMCLK <= '1';
  MSMDAT <= 'Z';
  MWAKE <= '0';
end toplevel;

