library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

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
  component FMUL is
    port (
      input1 : in  std_logic_vector (31 downto 0);
      input2 : in  std_logic_vector (31 downto 0);
      clk: in std_logic;
      output : out std_logic_vector (31 downto 0)
    );
  end component;
  signal multiplier_in1 : std_logic_vector(31 downto 0);
  signal multiplier_in2 : std_logic_vector(31 downto 0);
  signal multiplier_out : std_logic_vector(31 downto 0);
begin
  -- multiplier_in1 <= std_logic_vector(fp_in0);
  -- multiplier_in2 <= std_logic_vector(fp_in1);
  multiplier_in1 <=
    (others => '1') when TO_01(fp_in0, 'X')(0) = 'X' else
    std_logic_vector(fp_in0);
  multiplier_in2 <=
    (others => '1') when TO_01(fp_in1, 'X')(0) = 'X' else
    std_logic_vector(fp_in1);

  fmul_unit: FMUL
  port map (
    input1 => multiplier_in1,
    input2 => multiplier_in2,
    clk => clk,
    output => multiplier_out
  );

  fp_out <= unsigned(multiplier_out);
end architecture behavioral;
