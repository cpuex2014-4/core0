library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;
use work.kakeudon_fpu.all;

entity fp_multiplier is
  generic (
    debug_out : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in unsigned(1 downto 0);
    fp_in0 : in unsigned(31 downto 0);
    fp_in1 : in unsigned(31 downto 0);
    fp_out : out unsigned(31 downto 0));
end entity fp_multiplier;

architecture behavioral of fp_multiplier is
  signal multiplier_in1 : unsigned(31 downto 0);
  signal multiplier_in2 : unsigned(31 downto 0);
  signal multiplier_out : unsigned(31 downto 0);
begin
  multiplier_in1 <= fp_in0;
  multiplier_in2 <= fp_in1;

  fmul_unit: FMUL_OLD
  port map (
    input1 => multiplier_in1,
    input2 => multiplier_in2,
    clk => clk,
    output => multiplier_out
  );

  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      fp_out <= multiplier_out;
    end if;
  end process sequential;
end architecture behavioral;
