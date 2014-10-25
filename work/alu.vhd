library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity alu is
  port (
    alu_control : in unsigned(5 downto 0);
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
      case funct_t(to_integer(alu_control)) is
      when FUNCT_SLL | FUNCT_SLLV =>
        if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
          alu_out <= (others => 'X');
        else
          alu_out <= shift_left(alu_in1, to_integer(alu_in0(4 downto 0)));
        end if;
      when FUNCT_SRL | FUNCT_SRLV =>
        if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
          alu_out <= (others => 'X');
        else
          alu_out <= shift_right(alu_in1, to_integer(alu_in0(4 downto 0)));
        end if;
      when FUNCT_SRA | FUNCT_SRAV =>
        if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
          alu_out <= (others => 'X');
        else
          alu_out <= unsigned(
            shift_right(signed(alu_in1), to_integer(alu_in0(4 downto 0))));
        end if;
      when FUNCT_ADD | FUNCT_ADDU =>
        alu_out <= alu_in0 + alu_in1;
      when FUNCT_SUB | FUNCT_SUBU =>
        alu_out <= alu_in0 - alu_in1;
      when FUNCT_AND =>
        alu_out <= alu_in0 and alu_in1;
      when FUNCT_OR =>
        alu_out <= alu_in0 or alu_in1;
      when FUNCT_XOR =>
        alu_out <= alu_in0 xor alu_in1;
      when FUNCT_NOR =>
        alu_out <= not (alu_in0 or alu_in1);
      when FUNCT_SLT =>
        if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
          alu_out <= (others => 'X');
        elsif signed(alu_in0) < signed(alu_in1) then
          alu_out <= to_unsigned(1, 32);
        else
          alu_out <= to_unsigned(0, 32);
        end if;
      when FUNCT_SLTU =>
        if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
          alu_out <= (others => 'X');
        elsif alu_in0 < alu_in1 then
          alu_out <= to_unsigned(1, 32);
        else
          alu_out <= to_unsigned(0, 32);
        end if;
      when others =>
        alu_out <= (others => '-');
      end case;
    end if;
  end process combinational;
end architecture behavioral;

