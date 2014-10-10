library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package core is
  component cpu is
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
  end component cpu;
end package core;
