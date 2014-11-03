library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity reservation_station is
  generic (
    unit_name : string;
    latency : natural;
    num_entries : natural;
    opcode_len : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_opcode : in unsigned(opcode_len-1 downto 0);
    dispatch_operand0_available : in std_logic;
    dispatch_operand0_value : in unsigned_word;
    dispatch_operand0_tag : in tomasulo_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1';
    unit_available : in std_logic;
    issue : out std_logic := '0';
    issue_opcode : out unsigned(opcode_len-1 downto 0);
    issue_operand0 : out unsigned_word;
    issue_operand1 : out unsigned_word;
    broadcast_available : out std_logic := '0';
    broadcast_tag : out tomasulo_tag_t);
end entity reservation_station;

architecture behavioral of reservation_station is
  constant debug_out : boolean := true;

  subtype entry_id_t is integer range 0 to num_entries-1;
  signal dispatchable_entry_id : entry_id_t := 0;


  type broadcast_queue_t is array(0 to latency-1) of tomasulo_tag_t;
  signal broadcast_available_queue :
    std_logic_vector(0 to latency-1) := (others => '0');
  signal broadcast_tag_queue : broadcast_queue_t;

  type entries_opcode_t is
    array(0 to num_entries-1) of unsigned(opcode_len-1 downto 0);
  type entries_tag_t is array(0 to num_entries-1) of tomasulo_tag_t;
  type entries_operand_t is array(0 to num_entries-1) of unsigned_word;

  signal entries_busy :
    std_logic_vector(0 to num_entries-1) := (others => '0');
  signal entries_tag : entries_tag_t;
  signal entries_opcode : entries_opcode_t;
  signal entries_operand0_available : std_logic_vector(0 to num_entries-1);
  signal entries_operand0_value : entries_operand_t;
  signal entries_operand0_tag : entries_tag_t;
  signal entries_operand1_available : std_logic_vector(0 to num_entries-1);
  signal entries_operand1_value : entries_operand_t;
  signal entries_operand1_tag : entries_tag_t;
begin
  broadcast_available <= broadcast_available_queue(latency-1);
  broadcast_tag <= broadcast_tag_queue(latency-1);

  dispatchable <= not entries_busy(num_entries-1);

  sequential : process(clk, rst)
    -- temporary values for issuing
    variable entries_issuable : std_logic_vector(0 to num_entries-1);
    variable entries_issuable_accum : std_logic_vector(0 to num_entries-1);
    variable entries_issuable_any : std_logic;
    variable entries_issue_tag : entries_tag_t;
    variable entries_issue_opcode : entries_opcode_t;
    variable entries_issue_operand0 : entries_operand_t;
    variable entries_issue_operand1 : entries_operand_t;

    -- temporary values for snooping CDB
    type cdb_extended_id_array_t is
      array(0 to num_entries-1) of cdb_extended_id_t;
    variable next_entries_operand0_source : cdb_extended_id_array_t;
    variable next_entries_operand1_source : cdb_extended_id_array_t;

    variable next_entries_operand0_available : std_logic_vector(0 to num_entries-1);
    variable next_entries_operand0_value : entries_operand_t;
    variable next_entries_operand1_available : std_logic_vector(0 to num_entries-1);
    variable next_entries_operand1_value : entries_operand_t;

    subtype entry_select_t is integer range 0 to num_entries;
    variable next_dispatchable : std_logic;
    variable next_dispatchable_entry_id : entry_id_t;
    variable next_issuable_tag : entry_select_t;
  begin
    if rst = '1' then
      broadcast_available_queue <= (others => '0');
      entries_busy <= (others => '0');
    elsif rising_edge(clk) then
      -- assertions
      assert TO_X01(dispatch) /= 'X'
        report "RnSn for " & unit_name & ": " &
               "metavalue detected in dispatch"
          severity failure;
      for i in 0 to num_entries-1 loop
        assert TO_X01(entries_busy(i)) /= 'X'
          report "RnSn for " & unit_name & ": " &
                 "metavalue detected in entries_busy(" & integer'image(i) & ")"
            severity failure;
        if entries_busy(i) = '1' then
          assert TO_X01(entries_operand0_available(i)) /= 'X'
            report "RnSn for " & unit_name & ": " &
                   "metavalue detected in " &
                   "entries_operand0_available(" & integer'image(i) & ")"
              severity failure;
          assert TO_X01(entries_operand1_available(i)) /= 'X'
            report "RnSn for " & unit_name & ": " &
                   "metavalue detected in " &
                   "entries_operand1_available(" & integer'image(i) & ")"
              severity failure;
        end if;
      end loop;
      assert entries_busy(num_entries-1) = '0' or dispatch = '0'
        report "RnSn for " & unit_name & ": " &
               "invalid business condition in RnSn entries"
          severity failure;
      for i in 0 to num_entries-2 loop
        assert entries_busy(i) = '1' or entries_busy(i+1) = '0'
          report "RnSn for " & unit_name & ": " &
                 "invalid business condition in RnSn entries"
            severity failure;
      end loop;

      -- processing for issuing
      for i in 0 to num_entries-1 loop
        entries_issuable(i) :=
          entries_busy(i) and
          entries_operand0_available(i) and
          entries_operand1_available(i);
        if i = 0 then
          entries_issuable_accum(i) := entries_issuable(i);
        else
          entries_issuable_accum(i) :=
            entries_issuable(i) or entries_issuable_accum(i-1);
        end if;
      end loop;
      entries_issuable_any := entries_issuable_accum(num_entries-1);
      for i in num_entries-1 downto 0 loop
        if i = num_entries-1 or entries_issuable(i) = '1' then
          entries_issue_tag(i) := entries_tag(i);
          entries_issue_opcode(i) := entries_opcode(i);
          entries_issue_operand0(i) := entries_operand0_value(i);
          entries_issue_operand1(i) := entries_operand1_value(i);
        else
          entries_issue_tag(i) := entries_tag(i+1);
          entries_issue_opcode(i) := entries_opcode(i+1);
          entries_issue_operand0(i) := entries_operand0_value(i+1);
          entries_issue_operand1(i) := entries_operand1_value(i+1);
        end if;
      end loop;

      -- issue
      issue <= entries_issuable_any;
      issue_opcode <= entries_issue_opcode(0);
      issue_operand0 <= entries_issue_operand0(0);
      issue_operand1 <= entries_issue_operand1(0);
      broadcast_available_queue(0) <= entries_issuable_any;
      broadcast_tag_queue(0) <= entries_issue_tag(0);
      if entries_issuable_any = '1' then
        assert not debug_out
          report "RnSn for " & unit_name & ": " &
                 "issue (tag = " &
                   integer'image(to_integer(entries_issue_tag(0))) & ")"
            severity note;
      end if;

      -- snoop for CDB
      for i in 0 to num_entries-1 loop
        next_entries_operand0_source(i) := cdb_size;
        for j in 0 to cdb_size-1 loop
          if cdb_in_available(j) = '1' and
              cdb_in_tag(j) = entries_tag(i) then
            next_entries_operand0_source(i) := j;
          end if;
        end loop;
        if next_entries_operand0_source(i) = cdb_size then
          next_entries_operand0_available(i) := entries_operand0_available(i);
          next_entries_operand0_value(i) := entries_operand0_value(i);
        else
          next_entries_operand0_available(i) := '1';
          next_entries_operand0_value(i) :=
            cdb_in_value(next_entries_operand0_source(i));
        end if;

        next_entries_operand1_source(i) := cdb_size;
        for j in 0 to cdb_size-1 loop
          if cdb_in_available(j) = '1' and
              cdb_in_tag(j) = entries_tag(i) then
            next_entries_operand1_source(i) := j;
          end if;
        end loop;
        if next_entries_operand1_source(i) = cdb_size then
          next_entries_operand1_available(i) := entries_operand1_available(i);
          next_entries_operand1_value(i) := entries_operand1_value(i);
        else
          next_entries_operand1_available(i) := '1';
          next_entries_operand1_value(i) :=
            cdb_in_value(next_entries_operand1_source(i));
        end if;
      end loop;

      -- dispatch and shift
      for i in 0 to num_entries-1 loop
        if dispatch = '1' and
           ((entries_issuable_any = '1' and
            entries_busy(i) = '1' and
            (i = num_entries-1 or entries_busy(i+1) = '0')) or
           (entries_issuable_any = '0' and
            entries_busy(i) = '0' and
            (i = 0 or entries_busy(i-1) = '1'))) then
          assert TO_01(dispatch_tag, 'X')(0) /= 'X'
            report "RnSn for " & unit_name & ": " &
                   "metavalue detected in dispatch_tag"
              severity failure;
          assert TO_X01(dispatch_operand0_available) /= 'X'
            report "RnSn for " & unit_name & ": " &
                   "metavalue detected in dispatch_operand0_available"
              severity failure;
          assert TO_X01(dispatch_operand1_available) /= 'X'
            report "RnSn for " & unit_name & ": " &
                   "metavalue detected in dispatch_operand1_available"
              severity failure;
          assert not debug_out
            report "RnSn for " & unit_name & ": " &
                   "dispatch (tag = " &
                     integer'image(to_integer(dispatch_tag)) & ")"
              severity note;
          entries_busy(i) <= '1';
          entries_tag(i) <= dispatch_tag;
          entries_opcode(i) <= dispatch_opcode;
          entries_operand0_available(i) <= dispatch_operand0_available;
          entries_operand0_value(i) <= dispatch_operand0_value;
          entries_operand0_tag(i) <= dispatch_operand0_tag;
          entries_operand1_available(i) <= dispatch_operand1_available;
          entries_operand1_value(i) <= dispatch_operand1_value;
          entries_operand1_tag(i) <= dispatch_operand1_tag;
        elsif entries_issuable_accum(i) = '1' then
          if i = num_entries-1 then
            entries_busy(i) <= '0';
            entries_tag(i) <= (others => '-');
            entries_opcode(i) <= (others => '-');
            entries_operand0_available(i) <= '-';
            entries_operand0_value(i) <= (others => '-');
            entries_operand0_tag(i) <= (others => '-');
            entries_operand1_available(i) <= '-';
            entries_operand1_value(i) <= (others => '-');
            entries_operand1_tag(i) <= (others => '-');
          else
            entries_busy(i) <= entries_busy(i+1);
            entries_tag(i) <= entries_tag(i+1);
            entries_opcode(i) <= entries_opcode(i+1);
            entries_operand0_available(i) <=
              next_entries_operand0_available(i+1);
            entries_operand0_value(i) <= next_entries_operand0_value(i+1);
            entries_operand0_tag(i) <= entries_operand0_tag(i+1);
            entries_operand1_available(i) <=
              next_entries_operand1_available(i+1);
            entries_operand1_value(i) <= next_entries_operand1_value(i+1);
            entries_operand1_tag(i) <= entries_operand1_tag(i+1);
          end if;
        else
          entries_busy(i) <= entries_busy(i);
          entries_tag(i) <= entries_tag(i);
          entries_opcode(i) <= entries_opcode(i);
          entries_operand0_available(i) <= next_entries_operand0_available(i);
          entries_operand0_value(i) <= next_entries_operand0_value(i);
          entries_operand0_tag(i) <= entries_operand0_tag(i);
          entries_operand1_available(i) <= next_entries_operand1_available(i);
          entries_operand1_value(i) <= next_entries_operand1_value(i);
          entries_operand1_tag(i) <= entries_operand1_tag(i);
        end if;
      end loop;

      for i in 0 to latency-2 loop
        broadcast_available_queue(i+1) <= broadcast_available_queue(i);
        broadcast_tag_queue(i+1) <= broadcast_tag_queue(i);
      end loop;
    end if;
  end process sequential;

  debug_business_status : process(entries_busy)
  begin
    if debug_out then
      if TO_01(unsigned(entries_busy), 'X')(0) = 'X' then
        report "RnSn for " & unit_name & ": " &
               "entries_busy = X"
          severity note;
      else
        report "RnSn for " & unit_name & ": " &
               "entries_busy = " & bin_of_int(to_integer(unsigned(entries_busy)), entries_busy'length)
          severity note;
      end if;
    end if;
  end process;
end architecture behavioral;
