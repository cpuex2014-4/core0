library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity brancher is
  generic (
    debug_out : boolean;
    last_unit : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    refetch : in std_logic;
    brancher_in_available : in std_logic;
    brancher_in_tag : in tomasulo_tag_t;
    brancher_opcode : in unsigned(1 downto 0);
    brancher_in0 : in unsigned(31 downto 0);
    brancher_in1 : in unsigned(31 downto 0);
    brancher_in2 : in unsigned(31 downto 0);
    brancher_in3 : in unsigned(31 downto 0);
    brancher_out_available : out std_logic;
    brancher_out_value : out unsigned(31 downto 0);
    brancher_out_tag : out tomasulo_tag_t;
    cdb_writable : in std_logic;
    cdb_writable_next : out std_logic;
    brancher_unit_available : out std_logic);
end entity brancher;

architecture behavioral of brancher is
  signal brancher_available1 : std_logic := '0';
  signal brancher_value1 : unsigned(31 downto 0);
  signal brancher_tag1 : tomasulo_tag_t;
  signal cdb_use : std_logic;
begin
  cdb_use <= cdb_writable when last_unit else
             cdb_writable and brancher_available1;
  brancher_out_available <= brancher_available1 when cdb_use = '1' else 'Z';
  brancher_out_value <=
    brancher_value1 when cdb_use = '1' else (others => 'Z');
  brancher_out_tag <= brancher_tag1 when cdb_use = '1' else (others => 'Z');
  cdb_writable_next <= cdb_writable and not brancher_available1;
  brancher_unit_available <=
    cdb_writable or not brancher_available1 or not brancher_in_available;

  sequential: process(clk, rst)
    variable compar_result : std_logic;
    variable branch_result : unsigned(31 downto 0);
  begin
    if rising_edge(clk) then
      assert TO_X01(brancher_in_available) /= 'X'
        report "metavalue detected in brancher_in_available"
          severity failure;
      branch_result := (others => '-');
      if refetch = '1' then
        brancher_available1 <= '0';
        brancher_value1 <= (others => '-');
        brancher_tag1 <= (others => '-');
      elsif brancher_available1 = '1' and cdb_writable /= '1' then
        -- stall
      else
        if TO_01(brancher_opcode, 'X')(0) = 'X' or
           TO_01(brancher_in0, 'X')(0) = 'X' or
           TO_01(brancher_in1, 'X')(0) = 'X' then
          branch_result := (others => 'X');
        else
          if brancher_in0 = brancher_in1 then
            compar_result := '1';
          else
            compar_result := '0';
          end if;
          if (compar_result xor brancher_opcode(0)) = '1' then
            branch_result := brancher_in3;
          else
            branch_result := brancher_in2;
          end if;
        end if;
        brancher_available1 <= brancher_in_available;
        brancher_value1 <= branch_result;
        brancher_tag1 <= brancher_in_tag;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

