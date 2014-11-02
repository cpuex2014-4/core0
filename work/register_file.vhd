library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity register_file is
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    rd0_addr : in internal_register_t;
    rd0_available : out std_logic;
    rd0_value : out unsigned_word;
    rd0_tag : out tomasulo_tag_t;
    rd1_addr : in internal_register_t;
    rd1_available : out std_logic;
    rd1_value : out unsigned_word;
    rd1_tag : out tomasulo_tag_t;
    wr0_addr : in internal_register_t;
    wr0_enable : in std_logic;
    wr0_tag : in tomasulo_tag_t;
    wr1_addr_tag : in tomasulo_tag_t;
    wr1_enable : in std_logic;
    wr1_value : in unsigned_word);
end entity register_file;

architecture behavioral of register_file is
  constant debug_out : boolean := false;
  type reg_value_t is array(0 to 127) of unsigned_word;
  signal reg_value : reg_value_t := (others => x"00000000");
  type reg_tag_t is array(0 to 127) of tomasulo_tag_t;
  signal reg_tag : reg_tag_t;
  signal reg_available : std_logic_vector(0 to 127) := (others => '1');
begin
  rd0_available <= 'X' when TO_01(rd0_addr, 'X')(0) = 'X' else
                   reg_available(to_integer(rd0_addr));
  rd0_value     <= (others => 'X') when TO_01(rd0_addr, 'X')(0) = 'X' else
                   reg_value(to_integer(rd0_addr));
  rd0_tag       <= (others => 'X') when TO_01(rd0_addr, 'X')(0) = 'X' else
                   reg_tag(to_integer(rd0_addr));
  rd1_available <= 'X' when TO_01(rd1_addr, 'X')(0) = 'X' else
                   reg_available(to_integer(rd1_addr));
  rd1_value     <= (others => 'X') when TO_01(rd1_addr, 'X')(0) = 'X' else
                   reg_value(to_integer(rd1_addr));
  rd1_tag       <= (others => 'X') when TO_01(rd1_addr, 'X')(0) = 'X' else
                   reg_tag(to_integer(rd1_addr));
  sequential : process(clk, rst)
  begin
    if rst = '1' then
      reg_value <= (others => x"00000000");
      reg_tag <= (others => (others => '-'));
      reg_available <= (others => '1');
    elsif rising_edge(clk) then
    end if;
  end process sequential;
end architecture behavioral;

