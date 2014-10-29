library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity reservation_station is
  generic (
    latency : natural;
    num_entries : natural;
    opcode_len : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch_operand2_available : in std_logic;
    dispatch_operand2_value : in unsigned_word;
    dispatch_operand2_tag : in tomasulo_tag_t;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1';
    unit_available : in std_logic;
    issue : out std_logic := '0';
    issue_operand1 : out unsigned_word;
    issue_operand2 : out unsigned_word;
    broadcast_available : out std_logic;
    broadcast_tag : out tomasulo_tag_t);
end entity reservation_station;

architecture behavioral of reservation_station is
  constant debug_out : boolean := false;
  component reservation_entry is
    generic (
      opcode_len : natural);
    port (
      clk : in std_logic;
      rst : in std_logic;
      cdb_in_value : in cdb_in_value_t;
      cdb_in_tag : in cdb_in_tag_t;
      dispatch_operand1_available : in std_logic;
      dispatch_operand1_value : in unsigned_word;
      dispatch_operand1_tag : in tomasulo_tag_t;
      dispatch_operand2_available : in std_logic;
      dispatch_operand2_value : in unsigned_word;
      dispatch_operand2_tag : in tomasulo_tag_t;
      dispatch : in std_logic;
      dispatch_tag : in tomasulo_tag_t;
      ready : out std_logic := '0';
      busy : out std_logic := '0';
      operand1 : out unsigned_word;
      operand2 : out unsigned_word);
  end component reservation_entry;

  subtype entry_id_t is integer range 0 to num_entries-1;
  signal dispatchable_entry_id : entry_id_t := 0;

  type entries_operand_t is array(0 to num_entries-1) of unsigned_word;
  signal entries_dispatch : std_logic_vector(0 to num_entries-1);
  signal entries_ready : std_logic_vector(0 to num_entries-1);
  signal entries_busy : std_logic_vector(0 to num_entries-1);
  signal entries_operand1 : entries_operand_t;
  signal entries_operand2 : entries_operand_t;

  type broadcast_queue_t is array(0 to latency) of tomasulo_tag_t;
  signal broadcast_available_queue : std_logic_vector(0 to latency);
  signal broadcast_tag_queue : broadcast_queue_t;
begin
  broadcast_available <= broadcast_available_queue(latency);
  broadcast_tag <= broadcast_tag_queue(latency);

  entries: for i in 0 to num_entries-1 generate
    entry: reservation_entry
    generic map (
      opcode_len => opcode_len)
    port map (
      clk => clk,
      rst => rst,
      cdb_in_value => cdb_in_value,
      cdb_in_tag => cdb_in_tag,
      dispatch_operand1_available => dispatch_operand1_available,
      dispatch_operand1_value => dispatch_operand1_value,
      dispatch_operand1_tag => dispatch_operand1_tag,
      dispatch_operand2_available => dispatch_operand2_available,
      dispatch_operand2_value => dispatch_operand2_value,
      dispatch_operand2_tag => dispatch_operand2_tag,
      dispatch => entries_dispatch(i),
      dispatch_tag => dispatch_tag,
      ready => entries_ready(i),
      busy => entries_busy(i),
      operand1 => entries_operand1(i),
      operand2 => entries_operand2(i));

    entries_dispatch(i) <= dispatch when i = dispatchable_entry_id else '0';
  end generate entries;

  sequential : process(clk, rst)
    subtype entry_select_t is integer range 0 to num_entries;
    variable next_dispatchable : std_logic;
    variable next_dispatchable_entry_id : entry_id_t;
    variable next_issuable_tag : entry_select_t;
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      -- TODO: more efficient arbiter
      -- ready to dispatch?
      next_dispatchable := '0';
      next_dispatchable_entry_id := 0;
      for i in 0 to num_entries-1 loop
        if TO_X01(entries_busy(i)) = 'X' then
          report "metavalue detected in entries_busy(" &
                 integer'image(i) & ")"
            severity failure;
        elsif entries_busy(i) /= '1' then
          next_dispatchable := '1';
          next_dispatchable_entry_id := i;
        end if;
      end loop;
      dispatchable <= next_dispatchable;
      dispatchable_entry_id <= next_dispatchable_entry_id;

      -- ready to issue?
      next_issuable_tag := num_entries;
      for i in 0 to num_entries-1 loop
        if TO_X01(entries_ready(i)) = 'X' then
          report "metavalue detected in entries_ready(" &
                 integer'image(i) & ")"
            severity failure;
        elsif entries_ready(i) = '1' then
          next_issuable_tag := i;
        end if;
      end loop;

      -- issue!
      if next_issuable_tag = num_entries then
        issue <= '0';
        issue_operand1 <= (others => '-');
        issue_operand2 <= (others => '-');
        broadcast_available_queue(0) <= '0';
        broadcast_tag_queue(0) <= (others => '-');
      else
        issue <= '1';
        issue_operand1 <= entries_operand1(next_issuable_tag);
        issue_operand2 <= entries_operand2(next_issuable_tag);
        broadcast_available_queue(0) <= '1';
        broadcast_tag_queue(0) <=
          to_unsigned(next_issuable_tag, tomasulo_tag_t'length);
      end if;

      -- push broadcast queue
      for i in 1 to latency loop
        broadcast_available_queue(i) <= broadcast_available_queue(i-1);
        broadcast_tag_queue(i) <= broadcast_tag_queue(i-1);
      end loop;
    end if;
  end process sequential;
end architecture behavioral;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity reservation_entry is
  generic (
    opcode_len : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch_operand2_available : in std_logic;
    dispatch_operand2_value : in unsigned_word;
    dispatch_operand2_tag : in tomasulo_tag_t;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    ready : out std_logic := '0';
    busy : out std_logic := '0';
    operand1 : out unsigned_word;
    operand2 : out unsigned_word);
end entity reservation_entry;

architecture behavioral of reservation_entry is
  signal opcode : unsigned(opcode_len-1 downto 0);
  signal operation_tag : tomasulo_tag_t;
  signal operand1_available : std_logic;
  signal operand1_value : unsigned_word;
  signal operand1_tag : tomasulo_tag_t;
  signal operand2_available : std_logic;
  signal operand2_value : unsigned_word;
  signal operand2_tag : tomasulo_tag_t;

  signal internal_busy : std_logic := '0';
begin
  busy <= internal_busy or dispatch;
  operand1 <= operand1_value;
  operand2 <= operand2_value;

  sequential : process(clk, rst)
    variable next_operand1_cdb_id : cdb_extended_id_t;
    variable next_operand2_cdb_id : cdb_extended_id_t;
    variable next_busy : std_logic;
  begin
    if rst = '1' then
      internal_busy <= '0';
    elsif rising_edge(clk) then
      assert TO_X01(internal_busy) /= 'X'
        report "metavalue detected in internal_busy"
          severity failure;
      assert TO_X01(dispatch) /= 'X'
        report "metavalue detected in dispatch"
          severity failure;
      assert TO_X01(operand1_available) /= 'X'
        report "metavalue detected in operand1_available"
          severity failure;

      -- detect operands from CDB
      next_operand1_cdb_id := cdb_size;
      if operand1_available /= '1' then
        for j in 0 to cdb_size-1 loop
          if cdb_in_tag(j) = dispatch_operand1_tag then
            next_operand1_cdb_id := j;
          end if;
        end loop;
      end if;
      next_operand2_cdb_id := cdb_size;
      if operand2_available /= '1' then
        for j in 0 to cdb_size-1 loop
          if cdb_in_tag(j) = dispatch_operand2_tag then
            next_operand2_cdb_id := j;
          end if;
        end loop;
      end if;

      -- update operands
      if dispatch = '1' then
        operand1_available <= dispatch_operand1_available;
        operand1_value <= dispatch_operand1_value;
        operand1_tag <= dispatch_operand1_tag;
      elsif next_operand1_cdb_id /= cdb_size then
        operand1_available <= '1';
        operand1_value <= cdb_in_value(next_operand1_cdb_id);
      end if;
      if dispatch = '1' then
        operand2_available <= dispatch_operand2_available;
        operand2_value <= dispatch_operand2_value;
        operand2_tag <= dispatch_operand2_tag;
      elsif next_operand2_cdb_id /= cdb_size then
        operand2_available <= '1';
        operand2_value <= cdb_in_value(next_operand2_cdb_id);
      end if;
      if dispatch = '1' then
        operation_tag <= dispatch_tag;
      end if;

      -- ready?
      ready <= internal_busy and operand1_available and operand2_available;

      -- busy?
      if dispatch = '1' then
        next_busy := '1';
      else
        next_busy := internal_busy;
      end if;
      for j in 0 to cdb_size-1 loop
        if cdb_in_tag(j) = operation_tag then
          next_busy := '0';
        end if;
      end loop;
      internal_busy <= next_busy;
    end if;
  end process sequential;
end architecture behavioral;
