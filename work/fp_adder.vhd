library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;
use work.kakeudon_fpu.all;

entity fp_adder is
  generic (
    debug_out : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in unsigned(1 downto 0);
    fp_in0 : in unsigned(31 downto 0);
    fp_in1 : in unsigned(31 downto 0);
    fp_out : out unsigned(31 downto 0));
end entity fp_adder;

architecture behavioral of fp_adder is
  signal adder_in1 : unsigned(31 downto 0);
  signal adder_in2 : unsigned(31 downto 0);
  signal adder_out : unsigned(31 downto 0);
  signal opcode_delay1 : unsigned(1 downto 0);
  signal fp_in0_neg_delay1 : unsigned(31 downto 0);
begin
  adder_in1 <= fp_in0;
  adder_in2 <= (fp_in1(31) xor opcode(0)) & fp_in1(30 downto 0);

  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      fp_in0_neg_delay1 <=
        (opcode(0) xor fp_in0(31)) & fp_in0(30 downto 0);
      opcode_delay1 <= opcode;

      if TO_X01(opcode_delay1(1)) = 'X' then
        fp_out <= (others => 'X');
      elsif opcode_delay1(1) = '0' then
        fp_out <= adder_out;
      else
        fp_out <= fp_in0_neg_delay1;
      end if;
    end if;
  end process sequential;

  fadd_unit: FADD
  port map (
    input1 => adder_in1,
    input2 => adder_in2,
    clk => clk,
    output => adder_out
  );
end architecture behavioral;
