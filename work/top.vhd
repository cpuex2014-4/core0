library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.serial.all;

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

  signal dqsbuf : std_logic_vector(7 downto 0);
  signal ckbuf : std_logic_vector(1 downto 0);
  signal rst : std_logic;

  signal send_busy : std_logic;
  signal send_data : std_logic_vector(7 downto 0);
  signal send_go : std_logic := '0';
  signal recv_busy : std_logic;
  signal recv_data : std_logic_vector(7 downto 0);
  signal recv_done : std_logic;

  signal recv_buf : std_logic_vector(7 downto 0);
  signal recv_buf_e : std_logic := '0';

  signal sram_addr : unsigned(20 downto 0) := (others => '0');
  signal sram_addr2 : unsigned(20 downto 0) := (others => '0');
  signal sram_addr3 : unsigned(20 downto 0) := (others => '0');
  signal sram_addr4 : unsigned(20 downto 0) := (others => '0');
  signal sram_addr5 : unsigned(20 downto 0) := (others => '0');
  signal sram_d : unsigned(35 downto 0) := (others => '0');
  signal sram_wrong_cnt1 : unsigned(7 downto 0) := (others => '0');
  signal sram_wrong_cnt2 : unsigned(7 downto 0) := (others => '0');
begin
  send : process(clk)
    variable send_go_v : std_logic := '0';
  begin
    if rising_edge(clk) then
      send_go_v := '0';

      -- SRAM process
      if sram_addr4 = (20 downto 0 => '0') then
        sram_wrong_cnt2 <= sram_wrong_cnt1;
        sram_wrong_cnt1 <= (others => '0');
      end if;
      if sram_addr5(20) = '1' then
        if sram_addr5(19 downto 0) < 998765 then
          if sram_d(19 downto 0) /= sram_addr5(19 downto 0) then
            sram_wrong_cnt1 <= sram_wrong_cnt1 + 1;
          end if;
        end if;
      end if;
      sram_d <= unsigned(ZDP & ZD);
      XWA <= sram_addr(20);
      ZA <= std_logic_vector(sram_addr(19 downto 0));
      if sram_addr3(20) = '1' then
        ZD <= (others => 'Z');
        ZDP <= (others => 'Z');
      else
        ZD(19 downto 0) <= std_logic_vector(sram_addr3(19 downto 0));
        ZD(31 downto 20) <= (others => '0');
        ZDP <= (others => '0');
      end if;
      sram_addr <= sram_addr + 1;
      sram_addr2 <= sram_addr;
      sram_addr3 <= sram_addr2;
      sram_addr4 <= sram_addr3;
      sram_addr5 <= sram_addr4;

      -- send
      if recv_done = '1' and recv_buf_e /= '1' then
        recv_buf <= recv_data;
        recv_buf_e <= '1';
      end if;
      if send_busy /= '1' and recv_buf_e = '1' then
        -- send_data <= recv_buf;
        send_data <= std_logic_vector(sram_wrong_cnt2);
        send_go_v := '1';
        recv_buf_e <= '0';
      end if;
      send_go <= send_go_v;
    end if;
  end process send;
  uart : rs232c generic map (
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

  ib: IBUFG
  port map (
    i => MCLK1,
    o => imclk);
  rst <= XRST;
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
    -- rst => rst);
    rst => not XRST);
  bg0 : BUFG
  port map (
    i => clk0bgi,
    o => clk0);
  bg1 : BUFG
  port map (
    i => clkbgi,
    o => clk);

  ckouts : for cki in 0 to 1 generate
    ckout : OBUFDS -- capacitance, iostandard, slew
    port map (
      i => ckbuf(cki),
      o => CK(cki),
      ob => XCK(cki));
  end generate;

  dqs_outs : for dqsi in 0 to 7 generate
    dqs_out : OBUFDS -- capacitance, iostandard, slew
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
  -- XWA <= '1';

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

