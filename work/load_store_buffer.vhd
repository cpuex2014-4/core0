library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.kakeudon.all;

entity load_store_buffer is
  generic (
    num_stage1_entries : natural;
    num_stage2_entries : natural);
  port (
    clk : in std_logic;
    rst : in std_logic;
    refetch : in std_logic;
    cdb_in_available : in std_logic_vector(0 to cdb_size-1);
    cdb_in_value : in cdb_in_value_t;
    cdb_in_tag : in cdb_in_tag_t;
    dispatch_isstore : in std_logic;
    dispatch_operand0 : in value_or_tag_t;
    dispatch_operand2 : in unsigned_word;
    dispatch : in std_logic;
    dispatch_tag : in tomasulo_tag_t;
    dispatchable : out std_logic := '1';
    rob_top_committable : in std_logic;
    rob_top : in tomasulo_tag_t;
    ls_committable : out std_logic;
    issue : out std_logic := '0';
    issue_tag : out tomasulo_tag_t;
    issue_isstore : out std_logic;
    issue_operand0 : out unsigned_word);
end entity load_store_buffer;

architecture behavioral of load_store_buffer is
  constant debug_out : boolean := true;

  type stage1_entries_tag_t is
    array(0 to num_stage1_entries-1) of tomasulo_tag_t;
  type stage1_entries_word_t is
    array(0 to num_stage1_entries-1) of unsigned_word;
  type stage1_entries_operand_t is
    array(0 to num_stage1_entries-1) of value_or_tag_t;

  signal stage1_entries_busy :
    std_logic_vector(0 to num_stage1_entries-1)
      := (others => '0');
  signal stage1_entries_tag : stage1_entries_tag_t;
  signal stage1_entries_isstore : std_logic_vector(0 to num_stage1_entries-1);
  signal stage1_entries_operand0 : stage1_entries_operand_t;
  signal stage1_entries_operand2 : stage1_entries_word_t;

  type stage2_entries_tag_t is
    array(0 to num_stage2_entries-1) of tomasulo_tag_t;
  type stage2_entries_word_t is
    array(0 to num_stage2_entries-1) of unsigned_word;

  signal stage2_entries_busy :
    std_logic_vector(0 to num_stage2_entries-1)
      := (others => '0');
  signal stage2_entries_tag : stage2_entries_tag_t;
  signal stage2_entries_isstore : std_logic_vector(0 to num_stage2_entries-1);
  signal stage2_entries_operand0 : stage2_entries_word_t;

  function compar_unsigned(a:unsigned; b:unsigned) return std_logic is
  begin
    if TO_01(a, 'X')(a'left) = 'X' or TO_01(b, 'X')(b'left) = 'X' then
      return 'X';
    elsif a = b then
      return '1';
    else
      return '0';
    end if;
  end function compar_unsigned;
  function compar_addr(a:unsigned_word; b:unsigned_word) return std_logic is
  begin
    if TO_01(a, 'X')(0) = 'X' or TO_01(b, 'X')(0) = 'X' then
      return 'X';
    elsif a = b then
      return '1';
    else
      return a(31) and b(31);
    end if;
  end function compar_addr;
begin
  dispatchable <= not stage1_entries_busy(num_stage1_entries-1);
  sequential : process(clk, rst)
    variable stage1_entries_issuable : std_logic;

    variable next_stage1_entries_operand0 : stage1_entries_operand_t;

    variable stage2_dispatch : std_logic := '0';
    variable stage2_dispatch_isstore : std_logic;
    variable stage2_dispatch_tag : tomasulo_tag_t;
    variable stage2_dispatch_operand0 : unsigned_word;


    variable stage2_entries_issuable :
      std_logic_vector(0 to num_stage2_entries-1);
    variable stage2_entries_issuable_accum :
      std_logic_vector(0 to num_stage2_entries-1);
    variable stage2_entries_issuable_any : std_logic;
    variable stage2_entries_issue_tag : stage2_entries_tag_t;
    variable stage2_entries_issue_isstore :
      std_logic_vector(0 to num_stage2_entries-1);
    variable stage2_entries_issue_operand0 : stage2_entries_word_t;
  begin
    if rst = '1' then
      stage1_entries_busy <= (others => '0');
      stage1_entries_tag <= (others => (others => '-'));
      stage1_entries_isstore <= (others => '-');
      stage1_entries_operand0 <=
        (others => ('-', (others => '-'), (others => '-')));

      stage1_entries_operand2 <= (others => (others => '-'));
      stage2_entries_busy <= (others => '0');
      stage2_entries_tag <= (others => (others => '-'));
      stage2_entries_isstore <= (others => '-');
      stage2_entries_operand0 <= (others => (others => '-'));
    elsif rising_edge(clk) then
      assert TO_X01(dispatch) /= 'X'
        report "LSBuffer Stage 1: " &
               "metavalue detected in dispatch"
          severity failure;

      stage1_entries_issuable :=
        stage1_entries_busy(0) and
        stage1_entries_operand0(0).available and
        not stage2_entries_busy(num_stage2_entries-1);

      if refetch = '1' then
        stage2_dispatch := '0';
        stage2_dispatch_isstore := '-';
        stage2_dispatch_tag := (others => '-');
        stage2_dispatch_operand0 := (others => '-');
      else
        stage2_dispatch := stage1_entries_issuable;
        stage2_dispatch_isstore := stage1_entries_isstore(0);
        stage2_dispatch_tag := stage1_entries_tag(0);
        stage2_dispatch_operand0 :=
          stage1_entries_operand0(0).value +
          stage1_entries_operand2(0);
        -- issue
        if stage1_entries_issuable = '1' then
          assert not debug_out
            report "LSBuffer Stage 1: " &
                   "issue (isstore = " &
                     std_ulogic'image(stage1_entries_isstore(0)) & ", " &
                     "tag = " &
                     integer'image(to_integer(stage1_entries_tag(0))) & ", " &
                     "o0 = " &
                       hex_of_word(stage1_entries_operand0(0).value+
                                   stage1_entries_operand2(0)) & ")"
              severity note;
        end if;
      end if;

      -- snoop for CDB
      for i in 0 to num_stage1_entries-1 loop
        next_stage1_entries_operand0(i) :=
          snoop(stage1_entries_operand0(i),
            cdb_in_available, cdb_in_value, cdb_in_tag,
            debug_out,
            "LSBufer Stage 1: " &
            "entry tag " & dec_of_unsigned(stage1_entries_tag(i)));
      end loop;
      if refetch = '1' then
        for i in 0 to num_stage1_entries-1 loop
          stage1_entries_busy(i) <= '0';
          stage1_entries_tag(i) <= (others => '-');
          stage1_entries_isstore(i) <= '-';
          stage1_entries_operand0(i) <=
            ('-', (others => '-'), (others => '-'));
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
            assert TO_X01(dispatch_operand0.available) /= 'X'
              report "LSBuffer Stage 1: " &
                     "metavalue detected in dispatch_operand0.available"
                severity failure;
            assert not debug_out
              report "LSBuffer Stage 1: " &
                     "dispatch (isstore = " &
                       std_ulogic'image(dispatch_isstore) & ", " &
                       "tag = " &
                       integer'image(to_integer(dispatch_tag)) & ", " &
                       "o0 = " &
                         str_of_value_or_tag(dispatch_operand0) & ", " &
                       "o2 = " &
                         hex_of_word(dispatch_operand2) & ")"
                severity note;
            stage1_entries_busy(i) <= '1';
            stage1_entries_tag(i) <= dispatch_tag;
            stage1_entries_isstore(i) <= dispatch_isstore;
            stage1_entries_operand0(i) <=
              snoop(dispatch_operand0,
                    cdb_in_available, cdb_in_value, cdb_in_tag,
                    debug_out,
                    "LSBufer Stage 1: " &
                    "dispatch: entry tag " &
                      dec_of_unsigned(dispatch_tag));
            stage1_entries_operand2(i) <= dispatch_operand2;
          elsif stage1_entries_issuable = '1' then
            if i = num_stage1_entries-1 then
              stage1_entries_busy(i) <= '0';
              stage1_entries_tag(i) <= (others => '-');
              stage1_entries_isstore(i) <= '-';
              stage1_entries_operand0(i) <=
                ('-', (others => '-'), (others => '-'));
              stage1_entries_operand2(i) <= (others => '-');
            else
              stage1_entries_busy(i) <= stage1_entries_busy(i+1);
              stage1_entries_tag(i) <= stage1_entries_tag(i+1);
              stage1_entries_isstore(i) <= stage1_entries_isstore(i+1);
              stage1_entries_operand0(i) <=
                next_stage1_entries_operand0(i+1);
              stage1_entries_operand2(i) <= stage1_entries_operand2(i+1);
            end if;
          else
            stage1_entries_busy(i) <= stage1_entries_busy(i);
            stage1_entries_tag(i) <= stage1_entries_tag(i);
            stage1_entries_isstore(i) <= stage1_entries_isstore(i);
            stage1_entries_operand0(i) <=
              next_stage1_entries_operand0(i);
            stage1_entries_operand2(i) <= stage1_entries_operand2(i);
          end if;
        end loop;
      end if;





      assert TO_X01(stage2_dispatch) /= 'X'
        report "LSBuffer Stage 2: " &
               "metavalue detected in stage2_dispatch"
          severity failure;
      for i in 0 to num_stage2_entries-1 loop
        assert TO_X01(stage2_entries_busy(i)) /= 'X'
          report "LSBuffer Stage 2: " &
                 "metavalue detected in stage2_entries_busy(" &
                 integer'image(i) & ")"
            severity failure;
      end loop;
      assert stage2_entries_busy(num_stage2_entries-1) = '0' or
             stage2_dispatch = '0'
        report "LSBuffer Stage 2: " &
               "invalid business condition in LS stage2_entries"
          severity failure;
      for i in 0 to num_stage2_entries-2 loop
        assert stage2_entries_busy(i) = '1' or stage2_entries_busy(i+1) = '0'
          report "LSBuffer Stage 2: " &
                 "invalid business condition in LS stage2_entries"
            severity failure;
      end loop;

      -- processing for issuing
      for i in 0 to num_stage2_entries-1 loop
        stage2_entries_issuable(i) :=
          stage2_entries_busy(i) and
          (not (stage2_entries_isstore(i) or
                stage2_entries_operand0(i)(31)) or
           ((not stage2_entries_isstore(i) or
             rob_top_committable) and
            compar_unsigned(stage2_entries_tag(i), rob_top)));
        for j in 0 to i-1 loop
          stage2_entries_issuable(i) :=
            stage2_entries_issuable(i) and
            compar_addr(
              stage2_entries_operand0(i),
              stage2_entries_operand0(j));
        end loop;
        if i = 0 then
          stage2_entries_issuable_accum(i) := stage2_entries_issuable(i);
        else
          stage2_entries_issuable_accum(i) :=
            stage2_entries_issuable(i) or stage2_entries_issuable_accum(i-1);
        end if;
      end loop;
      stage2_entries_issuable_any := stage2_entries_issuable_accum(num_stage2_entries-1);
      for i in num_stage2_entries-1 downto 0 loop
        if i = num_stage2_entries-1 or stage2_entries_issuable(i) = '1' then
          stage2_entries_issue_tag(i) := stage2_entries_tag(i);
          stage2_entries_issue_isstore(i) := stage2_entries_isstore(i);
          stage2_entries_issue_operand0(i) := stage2_entries_operand0(i);
        elsif i = num_stage2_entries-1 then
          stage2_entries_issue_tag(i) := (others => '-');
          stage2_entries_issue_isstore(i) := '-';
          stage2_entries_issue_operand0(i) := (others => '-');
        else
          stage2_entries_issue_tag(i) := stage2_entries_issue_tag(i+1);
          stage2_entries_issue_isstore(i) := stage2_entries_issue_isstore(i+1);
          stage2_entries_issue_operand0(i) :=
            stage2_entries_issue_operand0(i+1);
        end if;
      end loop;

      if refetch = '1' then
        issue <= '0';
        issue_isstore <= '-';
        issue_operand0 <= (others => '-');
      else
        -- issue
        issue <= stage2_entries_issuable_any;
        issue_tag <= stage2_entries_issue_tag(0);
        issue_isstore <= stage2_entries_issue_isstore(0);
        issue_operand0 <= stage2_entries_issue_operand0(0);
        if stage2_entries_issuable_any = '1' then
          assert not debug_out
            report "LSBuffer Stage 2: " &
                     "issue (isstore = " &
                     std_ulogic'image(stage2_entries_issue_isstore(0)) & ", " &
                     "tag = " &
                     integer'image(to_integer(
                       stage2_entries_issue_tag(0))) & ", " &
                     "o0 = " & hex_of_word(stage2_entries_issue_operand0(0))
                       & ")"
              severity note;
        end if;
      end if;

      if refetch = '1' then
        for i in 0 to num_stage2_entries-1 loop
          stage2_entries_busy(i) <= '0';
          stage2_entries_tag(i) <= (others => '-');
          stage2_entries_isstore(i) <= '-';
          stage2_entries_operand0(i) <= (others => '-');
        end loop;
      else
        -- dispatch and shift
        for i in 0 to num_stage2_entries-1 loop
          if stage2_dispatch = '1' and
             ((stage2_entries_issuable_any = '1' and
              stage2_entries_busy(i) = '1' and
              (i = num_stage2_entries-1 or stage2_entries_busy(i+1) = '0')) or
             (stage2_entries_issuable_any = '0' and
              stage2_entries_busy(i) = '0' and
              (i = 0 or stage2_entries_busy(i-1) = '1'))) then
            assert TO_01(stage2_dispatch_tag, 'X')(0) /= 'X'
              report "LSBuffer Stage 2: " &
                     "metavalue detected in stage2_dispatch_tag"
                severity failure;
            assert not debug_out
              report "LSBuffer Stage 2: " &
                     "dispatch (isstore = " &
                       std_ulogic'image(stage2_dispatch_isstore) & ", " &
                       "tag = " &
                       dec_of_unsigned(stage2_dispatch_tag) & ", " &
                       "o0 = " & hex_of_word(stage2_dispatch_operand0) & ")"
                severity note;
            stage2_entries_busy(i) <= '1';
            stage2_entries_tag(i) <= stage2_dispatch_tag;
            stage2_entries_isstore(i) <= stage2_dispatch_isstore;
            stage2_entries_operand0(i) <= stage2_dispatch_operand0;
          elsif stage2_entries_issuable_accum(i) = '1' then
            if i = num_stage2_entries-1 then
              stage2_entries_busy(i) <= '0';
              stage2_entries_tag(i) <= (others => '-');
              stage2_entries_isstore(i) <= '-';
              stage2_entries_operand0(i) <= (others => '-');
            else
              stage2_entries_busy(i) <= stage2_entries_busy(i+1);
              stage2_entries_tag(i) <= stage2_entries_tag(i+1);
              stage2_entries_isstore(i) <= stage2_entries_isstore(i+1);
              stage2_entries_operand0(i) <= stage2_entries_operand0(i+1);
            end if;
          else
            stage2_entries_busy(i) <= stage2_entries_busy(i);
            stage2_entries_tag(i) <= stage2_entries_tag(i);
            stage2_entries_isstore(i) <= stage2_entries_isstore(i);
            stage2_entries_operand0(i) <= stage2_entries_operand0(i);
          end if;
        end loop;
      end if;
    end if;
  end process sequential;

  combinational: process(
      stage2_entries_busy,
      stage2_entries_isstore,
      rob_top_committable,
      stage2_entries_tag,
      rob_top)
    variable next_ls_committable : std_logic;
  begin
    next_ls_committable := '0';
    for i in 0 to num_stage2_entries-1 loop
      next_ls_committable :=
        next_ls_committable or
        (stage2_entries_busy(i) and
         stage2_entries_isstore(i) and
         (rob_top_committable and
          compar_unsigned(stage2_entries_tag(i), rob_top)));
    end loop;
    ls_committable <= next_ls_committable;
  end process combinational;
end architecture behavioral;
