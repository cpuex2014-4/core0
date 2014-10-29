library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity load_queue is
  generic (
    latency : natural;
    num_phase1_entries : natural;
    num_phase2_entries : natural;
    opcode_len : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch_operand2_value : in unsigned_word;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1';
    unit_available : in std_logic;
    issue : out std_logic := '0';
    issue_operand1 : out unsigned_word;
    issue_operand2 : out unsigned_word;
    broadcast_available : out std_logic;
    broadcast_tag : out tomasulo_tag_t);
end entity load_queue;

architecture behavioral of load_queue is
  constant debug_out : boolean := false;

  type phase1_entries_operand_t is
    array(0 to num_phase1_entries-1) of unsigned_word;
  type phase1_entries_tag_t is
    array(0 to num_phase1_entries-1) of tomasulo_tag_t;
  signal phase1_entries_operand1_available :
    std_logic_vector(0 to num_phase1_entries-1);
  signal phase1_entries_operand1 : phase1_entries_operand_t;
  signal phase1_entries_operand1_tag : phase1_entries_tag_t;
  signal phase1_entries_operand2 : phase1_entries_operand_t;
begin

  sequential : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      
    end if;
  end process sequential;
end architecture behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity load_store_phase1_entry is
end entity load_store_phase1_entry;

architecture behavioral of load_store_phase1_entry is
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch_operand2_value : in unsigned_word;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    ready : out std_logic := '0';
    busy : out std_logic := '0';
    operand1 : out unsigned_word;
    operand2 : out unsigned_word);
begin
end architecture behavioral;
