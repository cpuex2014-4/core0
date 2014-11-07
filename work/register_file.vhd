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
    wr1_addr : in internal_register_t;
    wr1_enable : in std_logic;
    wr1_value : in unsigned_word);
end entity register_file;

architecture behavioral of register_file is
  constant debug_out : boolean := true;
  constant num_entries : natural := 128;
  type reg_value_t is array(0 to num_entries-1) of unsigned_word;
  signal reg_value : reg_value_t := (others => x"00000000");
  type reg_tag_t is array(0 to num_entries-1) of tomasulo_tag_t;
  signal reg_tag : reg_tag_t;
  signal reg_available : std_logic_vector(0 to num_entries-1)
    := (others => '1');
begin
  update_rd0 :
  process(rd0_addr, reg_available, reg_value, reg_tag,
          cdb_in_available, cdb_in_value, cdb_in_tag)
    variable available : std_logic;
    variable value : unsigned_word;
    variable tag : tomasulo_tag_t;
    variable source : cdb_extended_id_t;
  begin
    if TO_01(rd0_addr, 'X')(0) = 'X' then
      rd0_available <= 'X';
      rd0_value <= (others => 'X');
      rd0_tag <= (others => 'X');
    else
      available := reg_available(to_integer(rd0_addr));
      value := reg_value(to_integer(rd0_addr));
      tag := reg_tag(to_integer(rd0_addr));
      if TO_X01(available) = 'X' then
        report "metavalue detected in available"
          severity failure;
      elsif available = '1' then
        rd0_available <= '1';
        rd0_value <= value;
        rd0_tag <= (others => '-');
      else
        assert TO_01(tag, 'X')(0) /= 'X'
          report "metavalue detected in tag"
            severity failure;

        source := cdb_size;
        for i in 0 to cdb_size-1 loop
          if cdb_in_available(i) = '1' and cdb_in_tag(i) = tag then
            source := i;
          end if;
        end loop;

        if source = cdb_size then
          rd0_available <= available;
          rd0_value <= value;
          rd0_tag <= tag;
        else
          rd0_available <= '1';
          rd0_value <= cdb_in_value(source);
          rd0_tag <= (others => '-');
        end if;
      end if;
    end if;
  end process update_rd0;
  update_rd1 :
  process(rd1_addr, reg_available, reg_value, reg_tag,
          cdb_in_available, cdb_in_value, cdb_in_tag)
    variable available : std_logic;
    variable value : unsigned_word;
    variable tag : tomasulo_tag_t;
    variable source : cdb_extended_id_t;
  begin
    if TO_01(rd1_addr, 'X')(0) = 'X' then
      rd1_available <= 'X';
      rd1_value <= (others => 'X');
      rd1_tag <= (others => 'X');
    else
      available := reg_available(to_integer(rd1_addr));
      value := reg_value(to_integer(rd1_addr));
      tag := reg_tag(to_integer(rd1_addr));
      if TO_X01(available) = 'X' then
        report "metavalue detected in available"
          severity failure;
      elsif available = '1' then
        rd1_available <= '1';
        rd1_value <= value;
        rd1_tag <= (others => '-');
      else
        assert TO_01(tag, 'X')(0) /= 'X'
          report "metavalue detected in tag"
            severity failure;

        source := cdb_size;
        for i in 0 to cdb_size-1 loop
          if cdb_in_available(i) = '1' and cdb_in_tag(i) = tag then
            source := i;
          end if;
        end loop;

        if source = cdb_size then
          rd1_available <= available;
          rd1_value <= value;
          rd1_tag <= tag;
        else
          rd1_available <= '1';
          rd1_value <= cdb_in_value(source);
          rd1_tag <= (others => '-');
        end if;
      end if;
    end if;
  end process update_rd1;
  sequential : process(clk, rst)
  begin
    if rst = '1' then
      reg_value <= (others => x"00000000");
      reg_tag <= (others => (others => '-'));
      reg_available <= (others => '1');
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
        assert TO_01(wr0_tag, 'X')(0) /= 'X'
          report "metavalue detected in wr0_tag"
            severity failure;
        assert not debug_out
          report name_of_internal_register(wr0_addr) &
                 " <- tag(" & integer'image(to_integer(wr0_tag)) & ")"
            severity note;
        reg_available(to_integer(wr0_addr)) <= '0';
        -- do not discard value, in order to rollback
        -- reg_value(to_integer(wr0_addr)) <= (others => '-');
        reg_tag(to_integer(wr0_addr)) <= wr0_tag;
      end if;
      if wr1_enable = '1' then
        assert TO_01(wr1_addr, 'X')(0) /= 'X'
          report "metavalue detected in wr1_addr"
            severity failure;
        assert TO_01(wr1_value, 'X')(0) /= 'X'
          report "metavalue detected in wr1_value"
            severity failure;
        if not (wr0_enable = '1' and wr0_addr = wr1_addr) then
          assert not debug_out
            report name_of_internal_register(wr1_addr) &
                   " <- " & hex_of_word(wr1_value)
              severity note;
          reg_available(to_integer(wr1_addr)) <= '1';
          reg_value(to_integer(wr1_addr)) <= wr1_value;
          reg_tag(to_integer(wr1_addr)) <= (others => '-');
        end if;
      end if;
      if refetch = '1' then
        for i in 0 to num_entries-1 loop
          reg_available(i) <= '1';
        end loop;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

