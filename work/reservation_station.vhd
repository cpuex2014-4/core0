library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity reservation_station is
  generic (
    debug_out : boolean;
    unit_name : string;
    latency : natural;
    num_entries : natural;
    num_operands : natural;
    opcode_len : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    refetch : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_opcode : in unsigned(opcode_len-1 downto 0);
    dispatch_operands : in value_or_tag_array_t(0 to num_operands-1);
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1';
    unit_available : in std_logic;
    issue : out std_logic := '0';
    issue_opcode : out unsigned(opcode_len-1 downto 0);
    issue_operands : out unsigned_word_array_t(0 to num_operands-1);
    broadcast_available : out std_logic := '0';
    broadcast_tag : out tomasulo_tag_t);
end entity reservation_station;

architecture behavioral of reservation_station is
  subtype entry_id_t is integer range 0 to num_entries-1;
  signal dispatchable_entry_id : entry_id_t := 0;


  type broadcast_queue_t is array(0 to latency-1) of tomasulo_tag_t;
  signal broadcast_available_queue :
    std_logic_vector(0 to latency-1) := (others => '0');
  signal broadcast_tag_queue : broadcast_queue_t;

  type entries_opcode_t is
    array(0 to num_entries-1) of unsigned(opcode_len-1 downto 0);
  type entries_tag_t is array(0 to num_entries-1) of tomasulo_tag_t;
  type entries_value_t is array(0 to num_entries-1)
    of unsigned_word_array_t(0 to num_operands-1);
  type entries_operand_t is array(0 to num_entries-1)
    of value_or_tag_array_t(0 to num_operands-1);

  signal entries_busy :
    std_logic_vector(0 to num_entries-1) := (others => '0');
  signal entries_tag : entries_tag_t;
  signal entries_opcode : entries_opcode_t;
  signal entries_operands : entries_operand_t;

  function str_of_operands(ops:unsigned_word_array_t) return string is
  begin
    if ops'length = 0 then
      return "";
    elsif ops'length = 1 then
      return "o0 = " & hex_of_word(ops(0));
    elsif ops'length = 2 then
      return "o0 = " & hex_of_word(ops(0)) & ", " &
             "o1 = " & hex_of_word(ops(1));
    elsif ops'length = 3 then
      return "o0 = " & hex_of_word(ops(0)) & ", " &
             "o1 = " & hex_of_word(ops(1)) & ", " &
             "o2 = " & hex_of_word(ops(2));
    elsif ops'length = 4 then
      return "o0 = " & hex_of_word(ops(0)) & ", " &
             "o1 = " & hex_of_word(ops(1)) & ", " &
             "o2 = " & hex_of_word(ops(2)) & ", " &
             "o3 = " & hex_of_word(ops(3));
    elsif ops'length = 5 then
      return "o0 = " & hex_of_word(ops(0)) & ", " &
             "o1 = " & hex_of_word(ops(1)) & ", " &
             "o2 = " & hex_of_word(ops(2)) & ", " &
             "o3 = " & hex_of_word(ops(3)) & ", " &
             "o4 = " & hex_of_word(ops(4));
    else
      return "o0 = " & hex_of_word(ops(0)) & ", " &
             "o1 = " & hex_of_word(ops(1)) & ", " &
             "o2 = " & hex_of_word(ops(2)) & ", " &
             "o3 = " & hex_of_word(ops(3)) & ", " &
             "o4 = " & hex_of_word(ops(4)) & "...";
    end if;
  end function str_of_operands;

  function str_of_operands(ops:value_or_tag_array_t) return string is
  begin
    if ops'length = 0 then
      return "";
    elsif ops'length = 1 then
      return "o0 = " & str_of_value_or_tag(ops(0));
    elsif ops'length = 2 then
      return "o0 = " & str_of_value_or_tag(ops(0)) & ", " &
             "o1 = " & str_of_value_or_tag(ops(1));
    elsif ops'length = 3 then
      return "o0 = " & str_of_value_or_tag(ops(0)) & ", " &
             "o1 = " & str_of_value_or_tag(ops(1)) & ", " &
             "o2 = " & str_of_value_or_tag(ops(2));
    elsif ops'length = 4 then
      return "o0 = " & str_of_value_or_tag(ops(0)) & ", " &
             "o1 = " & str_of_value_or_tag(ops(1)) & ", " &
             "o2 = " & str_of_value_or_tag(ops(2)) & ", " &
             "o3 = " & str_of_value_or_tag(ops(3));
    elsif ops'length = 5 then
      return "o0 = " & str_of_value_or_tag(ops(0)) & ", " &
             "o1 = " & str_of_value_or_tag(ops(1)) & ", " &
             "o2 = " & str_of_value_or_tag(ops(2)) & ", " &
             "o3 = " & str_of_value_or_tag(ops(3)) & ", " &
             "o4 = " & str_of_value_or_tag(ops(4));
    else
      return "o0 = " & str_of_value_or_tag(ops(0)) & ", " &
             "o1 = " & str_of_value_or_tag(ops(1)) & ", " &
             "o2 = " & str_of_value_or_tag(ops(2)) & ", " &
             "o3 = " & str_of_value_or_tag(ops(3)) & ", " &
             "o4 = " & str_of_value_or_tag(ops(4)) & "...";
    end if;
  end function str_of_operands;
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
    variable entries_issue_operands : entries_value_t;

    variable next_entries_operands : entries_operand_t;

    subtype entry_select_t is integer range 0 to num_entries;
    variable next_dispatchable : std_logic;
    variable next_dispatchable_entry_id : entry_id_t;
    variable next_issuable_tag : entry_select_t;
  begin
    if rst = '1' then
      issue <= '0';
      issue_opcode <= (others => '-');
      issue_operands <= (others => (others => '-'));
      broadcast_available_queue <= (others => '0');
      broadcast_tag_queue <= (others => (others => '-'));
      entries_busy <= (others => '0');
      entries_tag <= (others => (others => '-'));
      entries_opcode <= (others => (others => '-'));
      entries_operands <= (others => (others =>
        ('-', (others => '-'), (others => '-'))));
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
          for opid in 0 to num_operands-1 loop
            assert TO_X01(entries_operands(i)(opid).available) /= 'X'
              report "RnSn for " & unit_name & ": " &
                     "metavalue detected in " &
                     "entries_operands(" & integer'image(i) & ")(" &
                     integer'image(opid) & ").available"
                severity failure;
          end loop;
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
        entries_issuable(i) := entries_busy(i);
        for opid in 0 to num_operands-1 loop
          entries_issuable(i) :=
            entries_issuable(i) and entries_operands(i)(opid).available;
        end loop;
        if i = 0 then
          entries_issuable_accum(i) := entries_issuable(i);
        else
          entries_issuable_accum(i) :=
            entries_issuable(i) or entries_issuable_accum(i-1);
        end if;
      end loop;
      entries_issuable_any := entries_issuable_accum(num_entries-1);
      for i in num_entries-1 downto 0 loop
        if entries_issuable(i) = '1' then
          entries_issue_tag(i) := entries_tag(i);
          entries_issue_opcode(i) := entries_opcode(i);
          for opid in 0 to num_operands-1 loop
            entries_issue_operands(i)(opid) := entries_operands(i)(opid).value;
          end loop;
        elsif i = num_entries-1 then
          entries_issue_tag(i) := (others => '-');
          entries_issue_opcode(i) := (others => '-');
          for opid in 0 to num_operands-1 loop
            entries_issue_operands(i)(opid) := (others => '-');
          end loop;
        else
          entries_issue_tag(i) := entries_issue_tag(i+1);
          entries_issue_opcode(i) := entries_issue_opcode(i+1);
          for opid in 0 to num_operands-1 loop
            entries_issue_operands(i)(opid) :=
              entries_issue_operands(i+1)(opid);
          end loop;
        end if;
      end loop;

      if refetch = '1' then
        issue <= '0';
        issue_opcode <= (others => '-');
        issue_operands <= (others => (others => '-'));
        broadcast_available_queue(0) <= '0';
        broadcast_tag_queue(0) <= (others => '-');
      else
        -- issue
        issue <= entries_issuable_any;
        issue_opcode <= entries_issue_opcode(0);
        issue_operands <= entries_issue_operands(0);
        broadcast_available_queue(0) <= entries_issuable_any;
        broadcast_tag_queue(0) <= entries_issue_tag(0);
        if entries_issuable_any = '1' then
          assert not debug_out
            report "RnSn for " & unit_name & ": " &
                   "issue (opcode = " &
                     bin_of_int(to_integer(entries_issue_opcode(0)),
                       opcode_len) & ", " &
                     "tag = " &
                     integer'image(to_integer(entries_issue_tag(0))) & ", " &
                     str_of_operands(entries_issue_operands(0)) & ")"
              severity note;
        end if;
      end if;

      -- snoop for CDB
      for i in 0 to num_entries-1 loop
        for opid in 0 to num_operands-1 loop
          next_entries_operands(i)(opid) :=
            snoop(entries_operands(i)(opid),
                  cdb_in_available, cdb_in_value, cdb_in_tag,
                  debug_out,
                  "RnSn for " & unit_name & ": " &
                  "entry tag " & dec_of_unsigned(entries_tag(i)) &
                  ": operand" & integer'image(opid));
        end loop;
      end loop;

      if refetch = '1' then
        for i in 0 to num_entries-1 loop
          entries_busy(i) <= '0';
          entries_tag(i) <= (others => '-');
          entries_opcode(i) <= (others => '-');
          entries_operands(i) <= (others =>
            ('-', (others => '-'), (others => '-')));
        end loop;

        for i in 0 to latency-2 loop
          broadcast_available_queue(i+1) <= '0';
          broadcast_tag_queue(i+1) <= (others => '-');
        end loop;
      else
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
            for opid in 0 to num_operands-1 loop
              assert TO_X01(dispatch_operands(opid).available) /= 'X'
                report "RnSn for " & unit_name & ": " &
                       "metavalue detected in dispatch_operands(" &
                       integer'image(opid) & ").available"
                  severity failure;
            end loop;
            assert not debug_out
              report "RnSn for " & unit_name & ": " &
                     "dispatch (opcode = " &
                       bin_of_int(to_integer(dispatch_opcode), opcode_len) &
                         ", " &
                       "tag = " &
                       dec_of_unsigned(dispatch_tag) & ", " &
                       str_of_operands(dispatch_operands) & ")"
                severity note;
            entries_busy(i) <= '1';
            entries_tag(i) <= dispatch_tag;
            entries_opcode(i) <= dispatch_opcode;
            for opid in 0 to num_operands-1 loop
              entries_operands(i)(opid) <=
                snoop(dispatch_operands(opid),
                      cdb_in_available, cdb_in_value, cdb_in_tag,
                      debug_out,
                      "RnSn for " & unit_name & ": " &
                      "dispatch: entry tag " &
                        dec_of_unsigned(dispatch_tag) &
                      ": operand" & integer'image(opid));
            end loop;
          elsif entries_issuable_accum(i) = '1' then
            if i = num_entries-1 then
              entries_busy(i) <= '0';
              entries_tag(i) <= (others => '-');
              entries_opcode(i) <= (others => '-');
              entries_operands(i) <= (others =>
                ('-', (others => '-'), (others => '-')));
            else
              entries_busy(i) <= entries_busy(i+1);
              entries_tag(i) <= entries_tag(i+1);
              entries_opcode(i) <= entries_opcode(i+1);
              entries_operands(i) <= next_entries_operands(i+1);
            end if;
          else
            entries_busy(i) <= entries_busy(i);
            entries_tag(i) <= entries_tag(i);
            entries_opcode(i) <= entries_opcode(i);
            entries_operands(i) <= next_entries_operands(i);
          end if;
        end loop;

        for i in 0 to latency-2 loop
          broadcast_available_queue(i+1) <= broadcast_available_queue(i);
          broadcast_tag_queue(i+1) <= broadcast_tag_queue(i);
        end loop;
      end if;
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
