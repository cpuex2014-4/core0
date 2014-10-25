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
  constant debug_out : boolean := false;
  function debug_regname(i:integer) return string is
  begin
    case i is
    when  0 => return "$zero";
    when  1 => return "$at";
    when  2 => return "$v0";
    when  3 => return "$v1";
    when  4 => return "$a0";
    when  5 => return "$a1";
    when  6 => return "$a2";
    when  7 => return "$a3";
    when  8 => return "$t0";
    when  9 => return "$t1";
    when 10 => return "$t2";
    when 11 => return "$t3";
    when 12 => return "$t4";
    when 13 => return "$t5";
    when 14 => return "$t6";
    when 15 => return "$t7";
    when 16 => return "$s0";
    when 17 => return "$s1";
    when 18 => return "$s2";
    when 19 => return "$s3";
    when 20 => return "$s4";
    when 21 => return "$s5";
    when 22 => return "$s6";
    when 23 => return "$s7";
    when 24 => return "$t8";
    when 25 => return "$t9";
    when 26 => return "$k0";
    when 27 => return "$k1";
    when 28 => return "$gp";
    when 29 => return "$sp";
    when 30 => return "$fp";
    when 31 => return "$ra";
    when others => return "$" & integer'image(i);
    end case;
  end function debug_regname;
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
      gprs <= (others => x"00000000");
    elsif rising_edge(clk) then
      if gpr_we = '1' then
        if TO_01(gpr_wraddr, 'X')(0) = 'X' then
          report "metavalue detected in gpr_wraddr" severity warning;
        elsif gpr_wraddr /= 0 then
          if debug_out then
            if TO_01(gpr_wrval, 'X')(0) = 'X' then
              report "reg write: " &
                debug_regname(to_integer(gpr_wraddr)) & " X";
            else
              report "reg write: " &
                debug_regname(to_integer(gpr_wraddr)) & " " &
                integer'image(to_integer(signed(gpr_wrval)));
            end if;
          end if;
          gprs(to_integer(gpr_wraddr)) <= gpr_wrval;
        end if;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

