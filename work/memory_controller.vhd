library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity memory_controller is
  port (
    clk : in std_logic;
    addr : in unsigned(31 downto 0);
    data_write : in unsigned(31 downto 0);
    data_read : out unsigned(31 downto 0);
    we : in std_logic;
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
    ZZA : out std_logic); -- SRAM Sleep Mode
end entity memory_controller;

architecture behavioral of memory_controller is
  signal we_delay1 : std_logic;
  signal we_delay2 : std_logic;
  signal data_write_delay1 : unsigned(31 downto 0);
  signal data_write_delay2 : unsigned(31 downto 0);
begin
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
  ZA <= std_logic_vector(addr(21 downto 2));
  ZDP <= (others => 'Z') when we_delay2 /= '1' else (others => '0');
  ZD <= (others => 'Z') when we_delay2 /= '1' else
        std_logic_vector(data_write_delay2);
  data_read <= unsigned(ZD);
  XWA <= not we;
  sequential: process(clk)
  begin
    if rising_edge(clk) then
      we_delay1 <= we;
      we_delay2 <= we_delay1;
      data_write_delay1 <= data_write;
      data_write_delay2 <= data_write_delay1;
    end if;
  end process sequential;
end architecture behavioral;

