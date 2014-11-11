library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity load_store_buffer is
  generic (
    num_stage1_entries : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    refetch : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_isstore : in std_logic;
    dispatch_operand0_available : in std_logic;
    dispatch_operand0_value : in unsigned_word;
    dispatch_operand0_tag : in tomasulo_tag_t;
    dispatch_operand1_available : in std_logic;
    dispatch_operand1_value : in unsigned_word;
    dispatch_operand1_tag : in tomasulo_tag_t;
    dispatch_operand2 : in unsigned_word;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1');
end entity load_store_buffer;

architecture behavioral of load_store_buffer is
  constant debug_out : boolean := true;

  type stage1_entries_tag_t is
    array(0 to num_stage1_entries-1) of tomasulo_tag_t;
  type stage1_entries_operand_t is
    array(0 to num_stage1_entries-1) of unsigned_word;

  signal stage1_entries_busy :
    std_logic_vector(0 to num_stage1_entries-1)
      := (others => '0');
  signal stage1_entries_tag : stage1_entries_tag_t;
  signal stage1_entries_isstore : std_logic_vector(0 to num_stage1_entries-1);
  signal stage1_entries_operand0_available :
    std_logic_vector(0 to num_stage1_entries-1);
  signal stage1_entries_operand0_value : stage1_entries_operand_t;
  signal stage1_entries_operand0_tag : stage1_entries_tag_t;
  signal stage1_entries_operand1_available :
    std_logic_vector(0 to num_stage1_entries-1);
  signal stage1_entries_operand1_value : stage1_entries_operand_t;
  signal stage1_entries_operand1_tag : stage1_entries_tag_t;
  signal stage1_entries_operand2 : stage1_entries_operand_t;
begin
  dispatchable <= not stage1_entries_busy(num_stage1_entries-1);
  sequential : process(clk, rst)
    variable stage1_entries_issuable : std_logic;

    type stage1_cdb_extended_id_array_t is
      array(0 to num_stage1_entries-1) of cdb_extended_id_t;
    variable next_stage1_entries_operand0_source :
      stage1_cdb_extended_id_array_t;
    variable next_stage1_entries_operand1_source :
      stage1_cdb_extended_id_array_t;
    variable next_stage1_entries_busy :
      std_logic_vector(0 to num_stage1_entries-1)
        := (others => '0');
    variable next_stage1_entries_isstore :
      std_logic_vector(0 to num_stage1_entries-1);
    variable next_stage1_entries_operand0_available :
      std_logic_vector(0 to num_stage1_entries-1);
    variable next_stage1_entries_operand0_value : stage1_entries_operand_t;
    variable next_stage1_entries_operand0_tag : stage1_entries_tag_t;
    variable next_stage1_entries_operand1_available :
      std_logic_vector(0 to num_stage1_entries-1);
    variable next_stage1_entries_operand1_value : stage1_entries_operand_t;
    variable next_stage1_entries_operand1_tag : stage1_entries_tag_t;
    variable next_stage1_entries_operand2 : stage1_entries_operand_t;
  begin
    if rst = '1' then
      stage1_entries_busy <= (others => '0');
      stage1_entries_tag <= (others => (others => '-'));
      stage1_entries_isstore <= (others => '-');
      stage1_entries_operand0_available <= (others => '-');
      stage1_entries_operand0_value <= (others => (others => '-'));
      stage1_entries_operand0_tag <= (others => (others => '-'));
      stage1_entries_operand1_available <= (others => '-');
      stage1_entries_operand1_value <= (others => (others => '-'));
      stage1_entries_operand1_tag <= (others => (others => '-'));
      stage1_entries_operand2 <= (others => (others => '-'));
    elsif rising_edge(clk) then
      assert TO_X01(dispatch) /= 'X'
        report "LSBuffer Stage 1: " &
               "metavalue detected in dispatch"
          severity failure;

      stage1_entries_issuable :=
        stage1_entries_busy(0) and
        stage1_entries_operand0_available(0);

      if refetch = '1' then
      else
        -- issue
        if stage1_entries_issuable = '1' then
          assert not debug_out
            report "LSBuffer Stage 1: " &
                   "issue (isstore = " &
                     std_ulogic'image(stage1_entries_isstore(0)) & ", " &
                     "tag = " &
                     integer'image(to_integer(stage1_entries_tag(0))) & ", " &
                     "o0 = " &
                       hex_of_word(stage1_entries_operand0_value(0)+
                                   stage1_entries_operand2(0)) & ", " &
                     "o1 = " &
                       str_of_value_or_tag(
                         stage1_entries_operand1_available(0),
                         stage1_entries_operand1_value(0),
                         stage1_entries_operand1_tag(0)) & ")"
              severity note;
          report "TODO: next stage of Load/Store" severity failure;
        end if;
      end if;

      -- snoop for CDB
      for i in 0 to num_stage1_entries-1 loop
        next_stage1_entries_operand0_source(i) := cdb_size;
        if TO_X01(stage1_entries_operand0_available(i)) = 'X' then
          assert stage1_entries_busy(i) /= '1'
            report "LSBuffer Stage 1: " &
                   "metavalue detected in entries_operand0_available(" &
                   integer'image(i) & ")"
              severity failure;
        elsif stage1_entries_operand0_available(i) = '0' then
          for j in 0 to cdb_size-1 loop
            if cdb_in_available(j) = '1' and
                cdb_in_tag(j) = stage1_entries_operand0_tag(i) then
              next_stage1_entries_operand0_source(i) := j;
            end if;
          end loop;
        end if;
        if next_stage1_entries_operand0_source(i) = cdb_size then
          next_stage1_entries_operand0_available(i) := stage1_entries_operand0_available(i);
          next_stage1_entries_operand0_value(i) := stage1_entries_operand0_value(i);
        else
          assert not debug_out
            report "LSBuffer Stage 1: " &
                   "entry tag " &
                    integer'image(to_integer(stage1_entries_tag(i))) &
                   ": found operand0 from CDB (tag " &
                    integer'image(to_integer(stage1_entries_operand0_tag(i))) &
                    ")"
              severity note;
          next_stage1_entries_operand0_available(i) := '1';
          next_stage1_entries_operand0_value(i) :=
            cdb_in_value(next_stage1_entries_operand0_source(i));
        end if;

        next_stage1_entries_operand1_source(i) := cdb_size;
        if TO_X01(stage1_entries_operand1_available(i)) = 'X' then
          assert stage1_entries_busy(i) /= '1'
            report "LSBuffer Stage 1: " &
                   "metavalue detected in entries_operand1_available(" &
                   integer'image(i) & ")"
              severity failure;
        elsif stage1_entries_operand1_available(i) = '0' then
          for j in 0 to cdb_size-1 loop
            if cdb_in_available(j) = '1' and
                cdb_in_tag(j) = stage1_entries_operand1_tag(i) then
              next_stage1_entries_operand1_source(i) := j;
            end if;
          end loop;
        end if;
        if next_stage1_entries_operand1_source(i) = cdb_size then
          next_stage1_entries_operand1_available(i) := stage1_entries_operand1_available(i);
          next_stage1_entries_operand1_value(i) := stage1_entries_operand1_value(i);
        else
          assert not debug_out
            report "LSBuffer Stage 1: " &
                   "entry tag " &
                    integer'image(to_integer(stage1_entries_tag(i))) &
                   ": found operand1 from CDB (tag " &
                    integer'image(to_integer(stage1_entries_operand1_tag(i))) &
                    ")"
              severity note;
          next_stage1_entries_operand1_available(i) := '1';
          next_stage1_entries_operand1_value(i) :=
            cdb_in_value(next_stage1_entries_operand1_source(i));
        end if;
      end loop;
      if refetch = '1' then
        for i in 0 to num_stage1_entries-1 loop
          stage1_entries_busy(i) <= '0';
          stage1_entries_tag(i) <= (others => '-');
          stage1_entries_isstore(i) <= '-';
          stage1_entries_operand0_available(i) <= '-';
          stage1_entries_operand0_value(i) <= (others => '-');
          stage1_entries_operand0_tag(i) <= (others => '-');
          stage1_entries_operand1_available(i) <= '-';
          stage1_entries_operand1_value(i) <= (others => '-');
          stage1_entries_operand1_tag(i) <= (others => '-');
          stage1_entries_operand2(i) <= (others => '-');
        end loop;
      else
        -- dispatch and shift
        for i in 0 to num_stage1_entries-1 loop
          if dispatch = '1' and
             ((stage1_entries_issuable = '1' and
              stage1_entries_busy(i) = '1' and
              (i = num_stage1_entries-1 or stage1_entries_busy(i+1) = '0')) or
             (stage1_entries_issuable = '0' and
              stage1_entries_busy(i) = '0' and
              (i = 0 or stage1_entries_busy(i-1) = '1'))) then
            assert TO_01(dispatch_tag, 'X')(0) /= 'X'
              report "LSBuffer Stage 1: " &
                     "metavalue detected in dispatch_tag"
                severity failure;
            assert TO_X01(dispatch_operand0_available) /= 'X'
              report "LSBuffer Stage 1: " &
                     "metavalue detected in dispatch_operand0_available"
                severity failure;
            assert TO_X01(dispatch_operand1_available) /= 'X'
              report "LSBuffer Stage 1: " &
                     "metavalue detected in dispatch_operand1_available"
                severity failure;
            assert not debug_out
              report "LSBuffer Stage 1: " &
                     "dispatch (isstore = " &
                       std_ulogic'image(dispatch_isstore) & ", " &
                       "tag = " &
                       integer'image(to_integer(dispatch_tag)) & ", " &
                       "o0 = " &
                         str_of_value_or_tag(dispatch_operand0_available,
                                             dispatch_operand0_value,
                                             dispatch_operand0_tag) & ", " &
                       "o1 = " &
                         str_of_value_or_tag(dispatch_operand1_available,
                                             dispatch_operand1_value,
                                             dispatch_operand1_tag) & ", " &
                       "o2 = " &
                         hex_of_word(dispatch_operand2) & ")"
                severity note;
            stage1_entries_busy(i) <= '1';
            stage1_entries_tag(i) <= dispatch_tag;
            stage1_entries_isstore(i) <= dispatch_isstore;
            stage1_entries_operand0_available(i) <= dispatch_operand0_available;
            stage1_entries_operand0_value(i) <= dispatch_operand0_value;
            stage1_entries_operand0_tag(i) <= dispatch_operand0_tag;
            stage1_entries_operand1_available(i) <= dispatch_operand1_available;
            stage1_entries_operand1_value(i) <= dispatch_operand1_value;
            stage1_entries_operand1_tag(i) <= dispatch_operand1_tag;
            stage1_entries_operand2(i) <= dispatch_operand2;
          elsif stage1_entries_issuable = '1' then
            if i = num_stage1_entries-1 then
              stage1_entries_busy(i) <= '0';
              stage1_entries_tag(i) <= (others => '-');
              stage1_entries_isstore(i) <= '-';
              stage1_entries_operand0_available(i) <= '-';
              stage1_entries_operand0_value(i) <= (others => '-');
              stage1_entries_operand0_tag(i) <= (others => '-');
              stage1_entries_operand1_available(i) <= '-';
              stage1_entries_operand1_value(i) <= (others => '-');
              stage1_entries_operand1_tag(i) <= (others => '-');
              stage1_entries_operand2(i) <= (others => '-');
            else
              stage1_entries_busy(i) <= stage1_entries_busy(i+1);
              stage1_entries_tag(i) <= stage1_entries_tag(i+1);
              stage1_entries_isstore(i) <= stage1_entries_isstore(i+1);
              stage1_entries_operand0_available(i) <=
                next_stage1_entries_operand0_available(i+1);
              stage1_entries_operand0_value(i) <=
                next_stage1_entries_operand0_value(i+1);
              stage1_entries_operand0_tag(i) <=
                stage1_entries_operand0_tag(i+1);
              stage1_entries_operand1_available(i) <=
                next_stage1_entries_operand1_available(i+1);
              stage1_entries_operand1_value(i) <=
                next_stage1_entries_operand1_value(i+1);
              stage1_entries_operand1_tag(i) <=
                stage1_entries_operand1_tag(i+1);
              stage1_entries_operand2(i) <=
                next_stage1_entries_operand2(i+1);
            end if;
          else
            stage1_entries_busy(i) <= stage1_entries_busy(i);
            stage1_entries_tag(i) <= stage1_entries_tag(i);
            stage1_entries_isstore(i) <= stage1_entries_isstore(i);
            stage1_entries_operand0_available(i) <= next_stage1_entries_operand0_available(i);
            stage1_entries_operand0_value(i) <= next_stage1_entries_operand0_value(i);
            stage1_entries_operand0_tag(i) <= stage1_entries_operand0_tag(i);
            stage1_entries_operand1_available(i) <= next_stage1_entries_operand1_available(i);
            stage1_entries_operand1_value(i) <= next_stage1_entries_operand1_value(i);
            stage1_entries_operand1_tag(i) <= stage1_entries_operand1_tag(i);
            stage1_entries_operand2(i) <= next_stage1_entries_operand2(i);
          end if;
        end loop;
      end if;
      -- if dispatch = '1' then
      --   for i in 0 to num_stage1_entries-1 loop
      --     if i = 0 or
      --         (stage1_entries_busy(i-1) = '1' and
      --          stage1_entries_busy(i) = '0') then
      --       next_stage1_entries_busy(i) := '1';
      --       next_stage1_entries_isstore(i) := dispatch_isstore;
      --       next_stage1_entries_operand0_available(i) :=
      --         dispatch_operand0_available;
      --       next_stage1_entries_operand0_value(i) :=
      --         dispatch_operand0_value;
      --       next_stage1_entries_operand0_tag(i) :=
      --         dispatch_operand0_tag;
      --       next_stage1_entries_operand1_available(i) :=
      --         dispatch_operand1_available;
      --       next_stage1_entries_operand1_value(i) :=
      --         dispatch_operand1_value;
      --       next_stage1_entries_operand1_tag(i) :=
      --         dispatch_operand1_tag;
      --       next_stage1_entries_operand2(i) :=
      --         dispatch_operand2;
      --     else
      --     end if;
      --   end loop;
      -- end if;
    end if;
  end process sequential;
end architecture behavioral;
