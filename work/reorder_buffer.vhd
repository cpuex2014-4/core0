library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity reorder_buffer is
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
    rob_top_ready : out std_logic;
    rob_top_type : out rob_type_t;
    rob_top_dest : out internal_register_t;
    rob_top_value : out unsigned(31 downto 0);
    rob_bottom : out tomasulo_tag_t;
    rob_rd0_tag : in tomasulo_tag_t;
    rob_rd0_ready : out std_logic;
    rob_rd0_value : out unsigned(31 downto 0);
    rob_rd1_tag : in tomasulo_tag_t;
    rob_rd1_ready : out std_logic;
    rob_rd1_value : out unsigned(31 downto 0);
    commit : in std_logic);
end entity reorder_buffer;

architecture behavioral of reorder_buffer is
  constant debug_out : boolean := false;
  constant num_entries : natural := 16;

  signal rob_start : tomasulo_tag_t := (others => '0');
  signal rob_end : tomasulo_tag_t := (others => '0');

  type rob_entries_type_t is array(0 to num_entries-1) of rob_type_t;
  type rob_entries_dest_t is array(0 to num_entries-1) of internal_register_t;
  type rob_entries_val_t is array(0 to num_entries-1) of unsigned_word;
  signal rob_entries_type : rob_entries_type_t;
  signal rob_entries_dest : rob_entries_dest_t;
  signal rob_entries_val : rob_entries_val_t;
  signal rob_entries_ready : std_logic_vector(0 to num_entries-1);
begin
  rob_bottom <= rob_end;
  rob_top_ready <=
    rob_entries_ready(to_integer(rob_start)) when rob_start /= rob_end else
    '0';
  rob_top_type <= rob_entries_type(to_integer(rob_start));
  rob_top_dest <= rob_entries_dest(to_integer(rob_start));
  rob_top_value <= rob_entries_val(to_integer(rob_start));
  dispatchable <= '1' when rob_start - rob_end /= 1 else '0';

  update_rd0 : process(rob_rd0_tag, rob_entries_ready, rob_entries_val)
  begin
    if TO_01(rob_rd0_tag, 'X')(0) = 'X' then
      rob_rd0_ready <= 'X';
      rob_rd0_value <= (others => 'X');
    else
      rob_rd0_ready <= rob_entries_ready(to_integer(rob_rd0_tag));
      rob_rd0_value <= rob_entries_val(to_integer(rob_rd0_tag));
    end if;
  end process update_rd0;
  update_rd1 : process(rob_rd1_tag, rob_entries_ready, rob_entries_val)
  begin
    if TO_01(rob_rd1_tag, 'X')(0) = 'X' then
      rob_rd1_ready <= 'X';
      rob_rd1_value <= (others => 'X');
    else
      rob_rd1_ready <= rob_entries_ready(to_integer(rob_rd1_tag));
      rob_rd1_value <= rob_entries_val(to_integer(rob_rd1_tag));
    end if;
  end process update_rd1;

  sequential : process(clk, rst)
    type cdb_extended_id_array_t is
      array(0 to num_entries-1) of cdb_extended_id_t;
    variable result_source : cdb_extended_id_array_t;
  begin
    if rst = '1' then
      rob_start <= (others => '0');
      rob_end <= (others => '0');
    elsif rising_edge(clk) then
      assert TO_X01(dispatch) /= 'X'
        report "metavalue detected in dispatch"
          severity failure;
      assert TO_X01(commit) /= 'X'
        report "metavalue detected in commit"
          severity failure;

      if dispatch = '1' then
        rob_entries_type(to_integer(rob_end)) <= dispatch_type;
        rob_entries_dest(to_integer(rob_end)) <= dispatch_dest;
        rob_entries_ready(to_integer(rob_end)) <= '0';
        rob_entries_val(to_integer(rob_end)) <= (others => '-');
        rob_end <= rob_end + 1;
      end if;

      if commit = '1' then
        rob_start <= rob_start + 1;
      end if;

      -- snoop for CDB
      for i in 0 to num_entries-1 loop
        result_source(i) := cdb_size;
        for j in 0 to cdb_size-1 loop
          if cdb_in_available(j) = '1' and
              cdb_in_tag(j) = i then
            result_source(i) := j;
          end if;
        end loop;
        if result_source(i) < cdb_size then
          rob_entries_ready(i) <= '1';
          rob_entries_val(i) <= cdb_in_value(result_source(i));
        end if;
      end loop;
    end if;
  end process sequential;
end architecture behavioral;

