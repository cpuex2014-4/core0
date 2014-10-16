library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity alu is
  port (
    alu_control : in unsigned(3 downto 0);
    alu_in0 : in unsigned(31 downto 0);
    alu_in1 : in unsigned(31 downto 0);
    alu_out : buffer unsigned(31 downto 0);
    alu_iszero : out std_logic);
end entity alu;

architecture behavioral of alu is
begin
  alu_iszero <= 'X' when TO_01(alu_out, 'X')(31) = 'X' else
                '1' when alu_out = 0 else '0';
  combinational: process(alu_control, alu_in0, alu_in1)
  begin
    if TO_01(alu_control, 'X')(0) = 'X' then
      alu_out <= (others => 'X');
    else
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
  end process combinational;
end architecture behavioral;

