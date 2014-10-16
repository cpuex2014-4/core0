library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity alu is
  port (
    clk : in std_logic;
    alu_control : in unsigned(3 downto 0);
    alu_in0 : in unsigned(31 downto 0);
    alu_in1 : in unsigned(31 downto 0);
    alu_out : out unsigned(31 downto 0));
end entity alu;

architecture behavioral of alu is
begin
  sequential: process(clk)
  begin
    if rising_edge(clk) then
      case alu_control is
      when "0000" =>
        alu_out <= alu_in0 and alu_in1;
      when "0001" =>
        alu_out <= alu_in0 or alu_in1;
      when "0010" =>
        alu_out <= alu_in0 + alu_in1;
      when "0110" =>
        alu_out <= alu_in0 - alu_in1;
      when "0111" =>
        if signed(alu_in0) < signed(alu_in1) then
          alu_out <= to_unsigned(1, 32);
        else
          alu_out <= to_unsigned(0, 32);
        end if;
      when "1100" =>
        alu_out <= not (alu_in0 or alu_in1);
      when others =>
        alu_out <= (others => '0');
      end case;
    end if;
  end process sequential;
end architecture behavioral;

