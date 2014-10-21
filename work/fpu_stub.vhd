library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity fpu_stub is
  port (
    clk : in std_logic;
    fpu_control : in unsigned(5 downto 0);
    fpu_in0 : in unsigned(31 downto 0);
    fpu_in1 : in unsigned(31 downto 0);
    fpu_out : out unsigned(31 downto 0);
    fpu_condition : out std_logic);
end entity fpu_stub;

architecture behavioral of fpu_stub is
  signal fpu_in0_pipe1 : unsigned(31 downto 0);
  signal fpu_in1_pipe1 : unsigned(31 downto 0);
begin
  sequential: process(clk)
  begin
    if rising_edge(clk) then
      fpu_in0_pipe1 <= fpu_in0;
      fpu_in1_pipe1 <= fpu_in1;
    end if;
  end process sequential;

  combinational: process(fpu_control, fpu_in0, fpu_in1)
  begin
    if TO_01(fpu_control, 'X')(0) = 'X' then
      fpu_out <= (others => 'X');
    else
      case fpu_control is
      when "000000" => -- add.fmt
        fpu_out <= fpu_in0_pipe1 + fpu_in1_pipe1;
        fpu_condition <= '-';
      when "100000" => -- cvt.s.fmt
        fpu_out <= fpu_in1_pipe1;
        fpu_condition <= '-';
      when "100100" => -- cvt.w.fmt
        fpu_out <= fpu_in1_pipe1;
        fpu_condition <= '-';
      when others =>
        fpu_out <= (others => '-');
        fpu_condition <= '-';
      end case;
    end if;
  end process combinational;
end architecture behavioral;
