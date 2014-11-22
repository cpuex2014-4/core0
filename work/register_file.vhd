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
    refetch : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    rd0_addr : in internal_register_t;
    rd0 : out value_or_tag_t;
    rd1_addr : in internal_register_t;
    rd1 : out value_or_tag_t;
    wr0_addr : in internal_register_t;
    wr0_enable : in std_logic;
    wr0_tag : in tomasulo_tag_t;
    wr1_addr : in internal_register_t;
    wr1_enable : in std_logic;
    wr1_value : in unsigned_word);
end entity register_file;

architecture behavioral of register_file is
  constant debug_out : boolean := true;
  constant num_entries : natural := 128;
  type reg_t is array(0 to num_entries-1) of value_or_tag_t;
  signal reg : reg_t := (others =>
    ('1', x"00000000", (others => 'U')));
begin

  rd0 <= ('X', (others => 'X'), (others => 'X'))
         when TO_01(rd0_addr, 'X')(0) = 'X' else
         reg(to_integer(rd0_addr));
  rd1 <= ('X', (others => 'X'), (others => 'X'))
         when TO_01(rd1_addr, 'X')(0) = 'X' else
         reg(to_integer(rd1_addr));
  sequential : process(clk, rst)
  begin
    if rst = '1' then
      reg <= (others =>
               ('1', x"00000000", (others => '-')));
    elsif rising_edge(clk) then
      assert TO_X01(wr0_enable) /= 'X'
        report "metavalue detected in wr0_enable"
          severity failure;
      assert TO_X01(wr1_enable) /= 'X'
        report "metavalue detected in wr1_enable"
          severity failure;
      if wr0_enable = '1' then
        assert TO_01(wr0_addr, 'X')(0) /= 'X'
          report "metavalue detected in wr0_addr"
            severity failure;
        if wr0_addr /= 0 then
          assert TO_01(wr0_tag, 'X')(0) /= 'X'
            report "metavalue detected in wr0_tag"
              severity failure;
          assert not debug_out
            report name_of_internal_register(wr0_addr) &
                   " <- tag(" & integer'image(to_integer(wr0_tag)) & ")"
              severity note;
          reg(to_integer(wr0_addr)).available <= '0';
          -- do not discard value, in order to rollback
          -- reg(to_integer(wr0_addr)).value <= (others => '-');
          reg(to_integer(wr0_addr)).tag <= wr0_tag;
        end if;
      end if;
      if wr1_enable = '1' then
        assert TO_01(wr1_addr, 'X')(0) /= 'X'
          report "metavalue detected in wr1_addr"
            severity failure;
        if wr1_addr /= 0 then
          assert TO_01(wr1_value, 'X')(0) /= 'X'
            report "metavalue detected in wr1_value"
              severity failure;
          if not (wr0_enable = '1' and wr0_addr = wr1_addr) then
            assert not debug_out
              report name_of_internal_register(wr1_addr) &
                     " <- " & hex_of_word(wr1_value)
                severity note;
            reg(to_integer(wr1_addr)) <=
              ('1', wr1_value, (others => '-'));
          end if;
        end if;
      end if;
      if refetch = '1' then
        assert not debug_out
          report "refetch; " &
              "wr1_enable = " & std_ulogic'image(wr1_enable) & ", " &
              "wr1_addr = " & name_of_internal_register(wr1_addr) & ", " &
              "wr1_value = " & hex_of_word(wr1_value)
            severity note;
        for i in 0 to num_entries-1 loop
          reg(i).available <= '1';
        end loop;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

