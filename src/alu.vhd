library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity alu is
  generic (
    debug_out : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    alu_opcode : in unsigned(3 downto 0);
    alu_in0 : in unsigned(31 downto 0);
    alu_in1 : in unsigned(31 downto 0);
    alu_out : out unsigned(31 downto 0));
end entity alu;

architecture behavioral of alu is
begin
  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      if TO_01(alu_opcode, 'X')(0) = 'X' then
        alu_out <= (others => 'X');
      else
        case alu_opcode_t(to_integer(alu_opcode)) is
        when ALU_OP_ADD | ALU_OP_ADDU =>
          alu_out <= alu_in0 + alu_in1;
        when ALU_OP_SUB | ALU_OP_SUBU =>
          alu_out <= alu_in0 - alu_in1;
        when ALU_OP_AND =>
          alu_out <= alu_in0 and alu_in1;
        when ALU_OP_OR =>
          alu_out <= alu_in0 or alu_in1;
        when ALU_OP_XOR =>
          alu_out <= alu_in0 xor alu_in1;
        when ALU_OP_NOR =>
          alu_out <= not (alu_in0 or alu_in1);
        when ALU_OP_SLT =>
          if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
            alu_out <= (others => 'X');
          elsif signed(alu_in0) < signed(alu_in1) then
            alu_out <= to_unsigned(1, 32);
          else
            alu_out <= to_unsigned(0, 32);
          end if;
        when ALU_OP_SLTU =>
          if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
            alu_out <= (others => 'X');
          elsif alu_in0 < alu_in1 then
            alu_out <= to_unsigned(1, 32);
          else
            alu_out <= to_unsigned(0, 32);
          end if;
        when ALU_OP_SLL =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_out <= (others => 'X');
          else
            alu_out <= shift_left(alu_in1, to_integer(alu_in0(4 downto 0)));
          end if;
        when ALU_OP_SRL =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_out <= (others => 'X');
          else
            alu_out <= shift_right(alu_in1, to_integer(alu_in0(4 downto 0)));
          end if;
        when ALU_OP_SRA =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_out <= (others => 'X');
          else
            alu_out <= unsigned(
              shift_right(signed(alu_in1), to_integer(alu_in0(4 downto 0))));
          end if;
        when others =>
          alu_out <= (others => '-');
        end case;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

