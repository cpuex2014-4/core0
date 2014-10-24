library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- for debugging

library work;
use work.serial.all;
use work.kakeudon.all;

entity fp_register_file is
  port (
    clk : in std_logic;
    rst : in std_logic;
    fpr_rd0addr : in unsigned(4 downto 0);
    fpr_rd0val : out unsigned_word;
    fpr_rd1addr : in unsigned(4 downto 0);
    fpr_rd1val : out unsigned_word;
    fpr_wraddr : in unsigned(4 downto 0);
    fpr_wrval : in unsigned_word;
    fpr_we : in std_logic);
end entity fp_register_file;

architecture behavioral of fp_register_file is
  constant debug_out : boolean := true;
  function debug_regname(i:integer) return string is
  begin
    return "$f" & integer'image(i);
  end function debug_regname;
  function float_image(f:unsigned(31 downto 0)) return string is
    variable f_exp : integer;
    variable f_coef : integer;
    variable f_sgn_factor : integer;
    variable ff : real;
  begin
    f_exp := to_integer(f(30 downto 23));
    f_coef := to_integer(f(22 downto 0));
    if f_exp = 255 then
      if f_coef = 0 then
        return "Inf";
      else
        return "NaN";
      end if;
    end if;
    if f(31) = '1' then
      f_sgn_factor := -1;
    else
      f_sgn_factor := 1;
    end if;
    if f_exp = 0 then
      ff := real(f_sgn_factor * f_coef) * (2 ** real(-23-126));
    else
      ff := (real(f_sgn_factor * f_coef) * (2 ** real(-23)) + 1.0)
               * (2 ** real(f_exp-127));
    end if;
    return real'image(ff);
  end function float_image;
  type fprs_t is array(31 downto 0) of unsigned_word;
  signal fprs : fprs_t
    := (others => x"00000000");
begin
  fpr_rd0val <=
    (others => 'X') when TO_01(fpr_rd0addr, 'X')(4) = 'X' else
    fpr_wrval when fpr_we = '1' and fpr_rd0addr = fpr_wraddr else
    fprs(to_integer(fpr_rd0addr));
  fpr_rd1val <=
    (others => 'X') when TO_01(fpr_rd1addr, 'X')(4) = 'X' else
    fpr_wrval when fpr_we = '1' and fpr_rd1addr = fpr_wraddr else
    fprs(to_integer(fpr_rd1addr));

  sequential: process(clk, rst)
  begin
    if rst = '1' then
      fprs <= (others => x"00000000");
    elsif rising_edge(clk) then
      if fpr_we = '1' then
        if TO_01(fpr_wraddr, 'X')(0) = 'X' then
          report "metavalue detected in fpr_wraddr" severity warning;
        else
          if debug_out then
            if TO_01(fpr_wrval, 'X')(0) = 'X' then
              report "reg write: " &
                debug_regname(to_integer(fpr_wraddr)) & " X";
            else
              report "reg write: " &
                debug_regname(to_integer(fpr_wraddr)) & " " &
                float_image(fpr_wrval);
            end if;
          end if;
          fprs(to_integer(fpr_wraddr)) <= fpr_wrval;
        end if;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

