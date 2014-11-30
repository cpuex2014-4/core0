library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity fp_adder is
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in unsigned(1 downto 0);
    fp_in0 : in unsigned(31 downto 0);
    fp_in1 : in unsigned(31 downto 0);
    fp_out : out unsigned(31 downto 0));
end entity fp_adder;

architecture behavioral of fp_adder is
  component FADD is
    port (
      input1 : in  std_logic_vector (31 downto 0);
      input2 : in  std_logic_vector (31 downto 0);
      clk : in std_logic;
      output : out std_logic_vector (31 downto 0)
    );
  end component;
  signal adder_in1 : std_logic_vector(31 downto 0);
  signal adder_in2 : std_logic_vector(31 downto 0);
  signal adder_out : std_logic_vector(31 downto 0);
  signal opcode_delay1 : unsigned(1 downto 0);
  signal fp_in0_delay1 : unsigned(31 downto 0);
  signal opcode_delay2 : unsigned(1 downto 0);
  signal fp_in0_neg_delay2 : unsigned(31 downto 0);
begin
  -- adder_in1 <= std_logic_vector(fp_in0);
  -- adder_in2 <= std_logic_vector(
  --              (fp_in1(31) xor opcode(0)) & fp_in1(30 downto 0));
  adder_in1 <=
    (others => '1') when TO_01(fp_in0, 'X')(0) = 'X' else
    std_logic_vector(fp_in0);
  adder_in2 <=
    (others => '1') when TO_01(fp_in1, 'X')(0) = 'X' else
    std_logic_vector(
               (fp_in1(31) xor opcode(0)) & fp_in1(30 downto 0));

  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      fp_in0_delay1 <= fp_in0;
      opcode_delay1 <= opcode;
      fp_in0_neg_delay2 <=
        (opcode_delay1(0) xor fp_in0_delay1(31)) & fp_in0_delay1(30 downto 0);
      opcode_delay2 <= opcode_delay1;
    end if;
  end process sequential;

  fadd_unit: FADD
  port map (
    input1 => adder_in1,
    input2 => adder_in2,
    clk => clk,
    output => adder_out
  );

  fp_out <=
    (others => 'X') when TO_X01(opcode_delay2(1)) = 'X' else
    unsigned(adder_out) when opcode_delay2(1) = '0' else
    fp_in0_neg_delay2;
end architecture behavioral;
