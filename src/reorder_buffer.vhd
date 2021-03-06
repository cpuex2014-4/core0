library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity reorder_buffer is
  generic (
    debug_out : boolean;
    debug_out_commit : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatchable : out std_logic := '1';
    dispatch : in std_logic;
    dispatch_type : in rob_type_t;
    dispatch_dest : in internal_register_t;
    dispatch_rob_val : in value_or_tag_t;
    dispatch_branch : in value_or_tag_t;
    dispatch_predicted_branch : in unsigned(31 downto 0);
    dispatch_program_counter_plus1 : in unsigned(29 downto 0);
    dispatch_decode_success : in std_logic;
    dispatch_rasp : in rasp_t;
    rob_top_committable : out std_logic;
    rob_top : out tomasulo_tag_t;
    rob_top_type : out rob_type_t;
    rob_top_dest : out internal_register_t;
    rob_top_val : out value_or_tag_t;
    refetch : out std_logic;
    refetch_address : out unsigned(31 downto 0);
    refetch_rasp : out rasp_t;
    rob_bottom : out tomasulo_tag_t;
    rob_rd0_reg_tag : in tomasulo_tag_t;
    rob_rd0 : out value_or_tag_t;
    rob_rd1_reg_tag : in tomasulo_tag_t;
    rob_rd1 : out value_or_tag_t;
    commit : in std_logic);
end entity reorder_buffer;

architecture behavioral of reorder_buffer is
  constant num_entries : natural := 32;

  signal rob_start : tomasulo_tag_t := (others => '0');
  signal rob_end : tomasulo_tag_t := (others => '0');

  type rob_entries_type_t is array(0 to num_entries-1) of rob_type_t;
  type rob_entries_dest_t is array(0 to num_entries-1) of internal_register_t;
  type rob_entries_tag_t is array(0 to num_entries-1) of tomasulo_tag_t;
  type rob_entries_word_t is array(0 to num_entries-1) of unsigned_word;
  type rob_entries_value_or_tag_t is
    array(0 to num_entries-1) of value_or_tag_t;
  type rob_entries_rasp_t is array(0 to num_entries-1) of rasp_t;

  signal rob_entries_busy : std_logic_vector(0 to num_entries-1)
    := (others => '0');
  signal rob_entries_type : rob_entries_type_t;
  signal rob_entries_dest : rob_entries_dest_t;
  signal rob_entries_val : rob_entries_value_or_tag_t;

  signal rob_entries_branch : rob_entries_value_or_tag_t;
  signal rob_entries_predicted_branch : rob_entries_word_t;
  signal rob_entries_program_counter_plus1 : rob_entries_word_t;
  signal rob_entries_decode_success : std_logic_vector(0 to num_entries-1);
  signal rob_entries_rasp : rob_entries_rasp_t;

  signal internal_rob_top_committable : std_logic;
  signal internal_refetch : std_logic;
begin
  rob_top <= rob_start;
  rob_bottom <= rob_end;
  rob_top_committable <= internal_rob_top_committable;
  internal_rob_top_committable <=
    rob_entries_busy(to_integer(rob_start)) and
    rob_entries_val(to_integer(rob_start)).available and
    rob_entries_branch(to_integer(rob_start)).available;
  rob_top_type <= rob_entries_type(to_integer(rob_start));
  rob_top_dest <= rob_entries_dest(to_integer(rob_start));
  rob_top_val <= rob_entries_val(to_integer(rob_start));
  refetch <= internal_refetch;
  internal_refetch <=
    'X' when TO_X01(internal_rob_top_committable) = 'X' else
    '0' when internal_rob_top_committable /= '1' else
    'X' when
      TO_01(rob_entries_branch(to_integer(rob_start)).value, 'X')(0) = 'X' or
      TO_01(rob_entries_predicted_branch(to_integer(rob_start)), 'X')(0) = 'X'
    else
    '1' when
      rob_entries_branch(to_integer(rob_start)).value /=
      rob_entries_predicted_branch(to_integer(rob_start)) else '0';
  refetch_address <= rob_entries_branch(to_integer(rob_start)).value;
  refetch_rasp <= rob_entries_rasp(to_integer(rob_start));
  dispatchable <= not rob_entries_busy(to_integer(rob_end));

  rob_rd0 <=
    ('X', (others => 'X'), (others => 'X'))
      when TO_01(rob_rd0_reg_tag, 'X')(0) = 'X' else
    rob_entries_val(to_integer(rob_rd0_reg_tag));
  rob_rd1 <=
    ('X', (others => 'X'), (others => 'X'))
      when TO_01(rob_rd1_reg_tag, 'X')(0) = 'X' else
    rob_entries_val(to_integer(rob_rd1_reg_tag));

  sequential : process(clk, rst)
  begin
    if rst = '1' then
      rob_entries_busy <= (others => '0');
      rob_start <= (others => '0');
      rob_end <= (others => '0');
    elsif rising_edge(clk) then
      if internal_refetch = '1' then
        rob_entries_busy <= (others => '0');
        rob_start <= (others => '0');
        rob_end <= (others => '0');
      else
        assert TO_X01(dispatch) /= 'X'
          report "metavalue detected in dispatch"
            severity failure;
        assert TO_X01(commit) /= 'X'
          report "metavalue detected in commit"
            severity failure;

        if dispatch = '1' then
          assert TO_01(dispatch_dest, 'X')(0) /= 'X'
            report "metavalue detected in dispatch_dest"
              severity failure;
          assert rob_entries_busy(to_integer(rob_end)) = '0'
            report "busy bit of ROB dispatchee is not 0"
              severity failure;
          rob_entries_busy(to_integer(rob_end)) <= '1';
          rob_entries_type(to_integer(rob_end)) <= dispatch_type;
          rob_entries_dest(to_integer(rob_end)) <= dispatch_dest;
          assert TO_X01(dispatch_rob_val.available) /= 'X'
            report "metavalue detected in dispatch_rob_val.available"
              severity failure;
          rob_entries_val(to_integer(rob_end)) <=
            snoop(dispatch_rob_val,
                  cdb_in_available, cdb_in_value, cdb_in_tag,
                  debug_out,
                  "dispatch:ROB(" & dec_of_unsigned(rob_end) & ").val");

          rob_entries_branch(to_integer(rob_end)) <=
            snoop(dispatch_branch,
                  cdb_in_available, cdb_in_value, cdb_in_tag,
                  debug_out,
                  "dispatch:ROB(" & dec_of_unsigned(rob_end) & ").branch");
          rob_entries_predicted_branch(to_integer(rob_end)) <=
            dispatch_predicted_branch;
          rob_entries_program_counter_plus1(to_integer(rob_end)) <=
            dispatch_program_counter_plus1&"00";
          rob_entries_decode_success(to_integer(rob_end)) <=
            dispatch_decode_success;
          rob_entries_rasp(to_integer(rob_end)) <=
            dispatch_rasp;

          assert not debug_out
            report
                "ROB(" & dec_of_unsigned(rob_end) & ") <- " &
                "(type = " & rob_type_t'image(dispatch_type) & ", " &
                "dest = " & name_of_internal_register(dispatch_dest) & ", " &
                "val = " & str_of_value_or_tag(dispatch_rob_val) & ", " &
                "branch = " & str_of_value_or_tag(dispatch_branch) & ", " &
                "predict = " & hex_of_word(dispatch_predicted_branch) & ", " &
                "pc+4 = " &
                  hex_of_word(dispatch_program_counter_plus1&"00") & ", " &
                "decode_success = " &
                  std_ulogic'image(dispatch_decode_success) & ", " &
                "rasp = " &
                  dec_of_unsigned(dispatch_rasp) & ")"
              severity note;

          rob_end <= rob_end + 1;
        end if;

        if commit = '1' then
          assert rob_entries_decode_success(to_integer(rob_start)) = '1'
            report "commit: instruction decode failure: " &
                "tag(" & integer'image(to_integer(rob_start)) & "), " &
                "pc+4 = " &
                  hex_of_word(
                    rob_entries_program_counter_plus1(to_integer(rob_start)))
              severity failure;
          assert not debug_out_commit
            report "commit: " &
                "tag(" & integer'image(to_integer(rob_start)) & "), " &
                "pc+4 = " &
                  hex_of_word(
                    rob_entries_program_counter_plus1(to_integer(rob_start)))
              severity note;
          rob_entries_busy(to_integer(rob_start)) <= '0';
          rob_start <= rob_start + 1;
        end if;

        -- snoop for CDB
        for i in 0 to num_entries-1 loop
          if rob_entries_busy(i) = '1' then
            rob_entries_val(i) <=
              snoop(rob_entries_val(i),
                    cdb_in_available, cdb_in_value, cdb_in_tag,
                    debug_out,
                    "ROB(" & integer'image(i) & ").val");

            rob_entries_branch(i) <=
              snoop(rob_entries_branch(i),
                    cdb_in_available, cdb_in_value, cdb_in_tag,
                    debug_out,
                    "ROB(" & integer'image(i) & ").branch");
          end if;
        end loop;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

