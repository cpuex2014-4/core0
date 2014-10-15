library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

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
  signal mem_addr : unsigned(31 downto 0);
  signal mem_data_write : unsigned(31 downto 0);
  signal mem_data_read : unsigned(31 downto 0);
  signal mem_we : std_logic;

  signal rs_addr : unsigned(4 downto 0);
  signal rs_val : unsigned(31 downto 0);
  signal rt_addr : unsigned(4 downto 0);
  signal rt_val : unsigned(31 downto 0);
  signal rd_addr : unsigned(4 downto 0);
  signal rd_val : unsigned(31 downto 0);
  signal gpr_we : std_logic;

  signal rs232c_recv_empty : std_logic;
  signal rs232c_recv_top : unsigned(7 downto 0);
  signal rs232c_recv_consume : std_logic;
  signal rs232c_send_full : std_logic;
  signal rs232c_send_bottom : unsigned(7 downto 0);
  signal rs232c_send_push : std_logic;
begin
  core_unit : core
  port map (
    rs_addr => rs_addr,
    rs_val => rs_val,
    rt_addr => rt_addr,
    rt_val => rt_val,
    rd_addr => rd_addr,
    rd_val => rd_val,
    gpr_we => gpr_we,
    mem_addr => mem_addr,
    mem_data_write => mem_data_write,
    mem_data_read => mem_data_read,
    mem_we => mem_we,
    rs232c_recv_empty => rs232c_recv_empty,
    rs232c_recv_top => rs232c_recv_top,
    rs232c_recv_consume => rs232c_recv_consume,
    rs232c_send_full => rs232c_send_full,
    rs232c_send_bottom => rs232c_send_bottom,
    rs232c_send_push => rs232c_send_push,
    clk => clk,
    rst => rst);

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

  rs232c_unit : io_rs232c
  port map (
    clk => clk,
    rst => rst,
    RS_RX => RS_RX,
    RS_TX => RS_TX,
    recv_empty => rs232c_recv_empty,
    recv_top => rs232c_recv_top,
    recv_consume => rs232c_recv_consume,
    send_full => rs232c_send_full,
    send_bottom => rs232c_send_bottom,
    send_push => rs232c_send_push);
end behavioral;

