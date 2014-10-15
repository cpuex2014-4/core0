library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity register_file is
  port (
    clk : in std_logic;
    rst : in std_logic;
    gpr_rd0addr : in unsigned(4 downto 0);
    gpr_rd0val : out unsigned_word;
    gpr_rd1addr : in unsigned(4 downto 0);
    gpr_rd1val : out unsigned_word;
    gpr_wraddr : in unsigned(4 downto 0);
    gpr_wrval : in unsigned_word;
    gpr_we : in std_logic);
end entity register_file;

architecture behavioral of register_file is
  type gprs_t is array(31 downto 0) of unsigned_word;
  signal gprs : gprs_t
    := (others => x"00000000");
begin
  gpr_rd0val <=
    (others => 'X') when TO_01(gpr_rd0addr, 'X')(4) = 'X' else
    gpr_wrval when gpr_we = '1' and gpr_rd0addr = gpr_wraddr else
    gprs(to_integer(gpr_rd0addr));
  gpr_rd1val <=
    (others => 'X') when TO_01(gpr_rd1addr, 'X')(4) = 'X' else
    gpr_wrval when gpr_we = '1' and gpr_rd1addr = gpr_wraddr else
    gprs(to_integer(gpr_rd1addr));

  sequential: process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if gpr_we = '1' then
        gprs(to_integer(gpr_wraddr)) <= gpr_wrval;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

