library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;
use work.kakeudon_fpu.all;

entity fp_others is
  generic (
    debug_out : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    opcode : in unsigned(1 downto 0);
    fp_in0 : in unsigned(31 downto 0);
    fp_in1 : in unsigned(31 downto 0);
    fp_out : out unsigned(31 downto 0));
end entity fp_others;

architecture behavioral of fp_others is
  signal fdiv_out_6 : unsigned(31 downto 0);
  signal fsqrt_out_4 : unsigned(31 downto 0);
  signal itof_out_3 : unsigned(31 downto 0);
  signal ftoi_out_0 : unsigned(31 downto 0);
  signal fsqrt_out_5, fsqrt_out_6 : unsigned(31 downto 0);
  signal itof_out_4, itof_out_5, itof_out_6 : unsigned(31 downto 0);
  signal ftoi_out_1, ftoi_out_2, ftoi_out_3, ftoi_out_4, ftoi_out_5,
         ftoi_out_6 : unsigned(31 downto 0);
  signal opcode_1, opcode_2, opcode_3, opcode_4,
         opcode_5, opcode_6 : unsigned(1 downto 0);
begin
  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      fsqrt_out_5 <= fsqrt_out_4;
      fsqrt_out_6 <= fsqrt_out_5;
      itof_out_4 <= itof_out_3;
      itof_out_5 <= itof_out_4;
      itof_out_6 <= itof_out_5;
      ftoi_out_1 <= ftoi_out_0;
      ftoi_out_2 <= ftoi_out_1;
      ftoi_out_3 <= ftoi_out_2;
      ftoi_out_4 <= ftoi_out_3;
      ftoi_out_5 <= ftoi_out_4;
      ftoi_out_6 <= ftoi_out_5;
      opcode_1 <= opcode;
      opcode_2 <= opcode_1;
      opcode_3 <= opcode_2;
      opcode_4 <= opcode_3;
      opcode_5 <= opcode_4;
      opcode_6 <= opcode_5;

      if TO_01(opcode_6, 'X')(0) = 'X' then
        fp_out <= (others => 'X');
      elsif opcode_6 = "00" then
        fp_out <= fdiv_out_6;
      elsif opcode_6 = "01" then
        fp_out <= fsqrt_out_6;
      elsif opcode_6 = "10" then
        fp_out <= itof_out_6;
      else -- if opcode_6 = "11" then
        fp_out <= ftoi_out_6;
      end if;
    end if;
  end process sequential;

  fdiv_unit : FDIV
  port map (
    input1 => fp_in0,
    input2 => fp_in1,
    clk => clk,
    output => fdiv_out_6);

  fsqrt_unit : FSQRT
  port map (
    input => fp_in0,
    clk => clk,
    output => fsqrt_out_4);

  itof_unit : ITOF
  port map (
    input => fp_in0,
    clk => clk,
    output => itof_out_3);

  ftoi_unit : FTOI
  port map (
    input => fp_in0,
    output => ftoi_out_0);
end architecture behavioral;
