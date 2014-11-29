library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity fp_comparator is
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in unsigned(3 downto 0);
    fp_in0 : in unsigned(31 downto 0);
    fp_in1 : in unsigned(31 downto 0);
    fp_out : out unsigned(31 downto 0));
end entity fp_comparator;

architecture behavioral of fp_comparator is
begin
  sequential: process(clk, rst)
    variable a : unsigned(31 downto 0);
    variable b : unsigned(31 downto 0);
    variable both_neg : std_logic;
    variable c : std_logic;
  begin
    if rising_edge(clk) then
      if TO_01(fp_in0, 'X')(0) = 'X' or
         TO_01(fp_in1, 'X')(0) = 'X' then
        fp_out <= (others => 'X');
      else
        if fp_in0(30 downto 23) = "00000000" then
          a := x"00000000";
        else
          a := fp_in0;
        end if;
        if fp_in1(30 downto 23) = "00000000" then
          b := x"00000000";
        else
          b := fp_in1;
        end if;
        both_neg := a(31) and b(31);
        if (a(30 downto 23) = "11111111" and a(22 downto 0) = 0) or
           (a(30 downto 23) = "11111111" and a(22 downto 0) = 0) then
          c := opcode(0);
        else
          if signed(a) < signed(b) then
            if both_neg = '1' then
              c := '0';
            else
              c := opcode(2);
            end if;
          elsif a = b then
            c := opcode(1);
          else
            if both_neg = '1' then
              c := opcode(2);
            else
              c := '0';
            end if;
          end if;
        end if;
        fp_out <= (31 downto 1 => '0', 0 => c);
      end if;
    end if;
  end process;
end architecture behavioral;
