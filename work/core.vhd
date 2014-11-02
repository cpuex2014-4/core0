library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon_fpu.all;
use work.kakeudon.all;

entity core is
  port (
    -- Memory Controller
    mem_addr : out unsigned(29 downto 0);
    mem_data_write : out unsigned(31 downto 0);
    mem_data_read : in unsigned(31 downto 0);
    mem_we : out std_logic;
    -- RS-232C I/O Controller
    rs232c_recv_empty : in std_logic;
    rs232c_recv_top : in unsigned(7 downto 0);
    rs232c_recv_consume : out std_logic := '0';
    rs232c_send_full : in std_logic;
    rs232c_send_bottom : out unsigned(7 downto 0);
    rs232c_send_push : out std_logic := '0';
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);
end entity core;

architecture behavioral of core is
  constant debug_instruction_fetch : boolean := true;
  constant debug_cdb : boolean := true;

  signal cdb_available : std_logic_vector(0 to cdb_size-1);
  signal cdb_value : cdb_in_value_t;
  signal cdb_tag : cdb_in_tag_t;

  type instruction_memory_t is
    array(0 to 16#ffff#) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;
  attribute ram_style of instruction_memory: signal is "block";
  signal instruction_rom : instruction_rom_t := instruction_rom_data;
  attribute ram_style of instruction_rom: signal is "block";

  signal instruction_register_from_memory : unsigned(31 downto 0);
  signal instruction_register_from_rom : unsigned(31 downto 0);

  signal program_counter : unsigned(29 downto 0)
    := "101111111100000000000000000000"; -- 0xBFC00000

  -- instruction fetch
  signal program_counter_plus1 : unsigned(29 downto 0);
  signal instruction_selector : unsigned(1 downto 0);
  signal instruction_register : unsigned(31 downto 0);
  signal instruction_register_available : std_logic := '0';

  -- instruction decode
  type unit_id_t is (unit_alu, unit_fadd);
  signal unit_id : unit_id_t;
  signal unit_operation : unsigned(3 downto 0);
  signal operand0_addr : internal_register_t;
  signal operand1_addr : internal_register_t;
  signal immediate_val : unsigned(31 downto 0);
  signal use_immediate : std_logic;
  signal destination_addr : internal_register_t;
  signal destination_enable : std_logic;
  signal decoded_instruction_available : std_logic := '0';

  -- dispatch
  signal dispatch_operand0_available : std_logic;
  signal dispatch_operand0_value : unsigned_word;
  signal dispatch_operand0_tag : tomasulo_tag_t;
  signal dispatch_operand1_available_reg : std_logic;
  signal dispatch_operand1_value_reg : unsigned_word;
  signal dispatch_operand1_tag_reg : tomasulo_tag_t;
  signal dispatch_operand1_available : std_logic;
  signal dispatch_operand1_value : unsigned_word;
  signal dispatch_operand1_tag : tomasulo_tag_t;
  signal dispatch_tag : tomasulo_tag_t;

  signal any_dispatch : std_logic;

  signal alu_dispatchable : std_logic;
  signal alu_dispatch : std_logic := '0';
  signal alu_available : std_logic;
  signal alu_issue : std_logic;
  signal alu_opcode : unsigned(2 downto 0);
  signal alu_operand0 : unsigned(31 downto 0);
  signal alu_operand1 : unsigned(31 downto 0);

  -- reorder buffer
  signal rob_top : tomasulo_tag_t;
  signal rob_top_value : unsigned(31 downto 0);
  signal rob_bottom : tomasulo_tag_t;
  signal rob_pop : std_logic;
begin
  instruction_register <=
    (others => 'X') when TO_01(instruction_selector, 'X')(0) = 'X' else
    instruction_register_from_memory
      when instruction_selector = "00" else
    instruction_register_from_rom
      when instruction_selector = "01" else
    (others => '-');

  instruction_fetch_brom_sequential : process(clk)
  begin
    if rising_edge(clk) then
      -- instruction_register_from_memory <=
      --   instruction_memory(to_integer(program_counter(15 downto 0)));
      instruction_register_from_rom <=
        instruction_rom(to_integer(program_counter(4 downto 0)));
    end if;
  end process instruction_fetch_brom_sequential;
  instruction_fetch_sequential : process(clk, rst)
  begin
    if rst = '1' then
      program_counter <= "101111111100000000000000000000";
      program_counter_plus1 <= (others => '-');
      instruction_selector <= (others => '-');
      instruction_register_available <= '0';
    elsif rising_edge(clk) then
      assert TO_01(program_counter, 'X')(0) /= 'X'
        report "metavalue detected in program_counter"
          severity failure;
      assert not debug_instruction_fetch
        report "program_counter = " & hex_of_word(program_counter&"00")
          severity note;
      if program_counter(26 downto 16) = "00000000000" then
        instruction_selector <= "00";
      elsif program_counter(26 downto 5) = "1111111000000000000000" then
        instruction_selector <= "01";
      else
        instruction_selector <= "10";
      end if;
      program_counter <= program_counter + 1;
      program_counter_plus1 <= program_counter + 1;
      instruction_register_available <= '1';
    end if;
  end process instruction_fetch_sequential;

  rs232c_send_bottom <= instruction_register(7 downto 0);

  decode_sequential : process(clk, rst)
    variable d_opcode : opcode_t;
    variable d_funct : funct_t;
    variable d_rs : internal_register_t;
    variable d_rt : internal_register_t;
    variable d_rd : internal_register_t;
    variable d_sa : unsigned(4 downto 0);
    variable d_fmt : unsigned(4 downto 0);
    variable d_fs : internal_register_t;
    variable d_ft : internal_register_t;
    variable d_fd : internal_register_t;
    variable d_short_imm : unsigned(15 downto 0);
    variable d_sign_ext_imm : unsigned(31 downto 0);
    variable d_zero_ext_imm : unsigned(31 downto 0);
  begin
    if rst = '1' then
      unit_id <= unit_alu; -- don't care
      unit_operation <= (others => '-');
      operand0_addr <= (others => '-');
      operand1_addr <= (others => '-');
      immediate_val <= (others => '-');
      use_immediate <= '-';
      destination_addr <= (others => '-');
      destination_enable <= '-';
      decoded_instruction_available <= '0';
    elsif rising_edge(clk) then
      if instruction_register_available = '1' then
        assert TO_01(instruction_register, 'X')(0) /= 'X'
          report "metavalue detected in instruction_register"
            severity failure;
        d_opcode := to_integer(instruction_register(31 downto 26));
        d_funct := to_integer(instruction_register(5 downto 0));
        d_rs := "00" & instruction_register(25 downto 21);
        d_rt := "00" & instruction_register(20 downto 16);
        d_rd := "00" & instruction_register(15 downto 11);
        d_sa := instruction_register(10 downto 6);
        d_fmt := instruction_register(25 downto 21);
        d_ft := "01" & instruction_register(20 downto 16);
        d_fs := "01" & instruction_register(15 downto 11);
        d_fd := "01" & instruction_register(10 downto 6);
        d_short_imm := instruction_register(15 downto 0);
        d_sign_ext_imm := unsigned(resize(signed(d_short_imm), 32));
        d_zero_ext_imm := resize(d_short_imm, 32);
        case d_opcode is
        when OP_ADDIU =>
          unit_id <= unit_alu;
          unit_operation <= "0001";
          operand0_addr <= d_rs;
          operand1_addr <= (others => '-');
          immediate_val <= d_sign_ext_imm;
          use_immediate <= '1';
          destination_addr <= d_rd;
          destination_enable <= '-';
          decoded_instruction_available <= '1';
        when OP_JAL =>
          -- TODO: correct control signal
          unit_id <= unit_alu;
          unit_operation <= "0001";
          operand0_addr <= d_rs;
          operand1_addr <= (others => '-');
          immediate_val <= d_sign_ext_imm;
          use_immediate <= '1';
          destination_addr <= d_rd;
          destination_enable <= '-';
          decoded_instruction_available <= '1';
        when others =>
          report "unknown opcode " & bin_of_int(d_opcode,6)
            severity failure;
          unit_id <= unit_alu; -- don't care
          unit_operation <= (others => '-');
          operand0_addr <= (others => '-');
          operand1_addr <= (others => '-');
          immediate_val <= (others => '-');
          use_immediate <= '-';
          destination_addr <= (others => '-');
          destination_enable <= '-';
          decoded_instruction_available <= '0';
        end case;
      else
        unit_id <= unit_alu; -- don't care
        unit_operation <= (others => '-');
        operand0_addr <= (others => '-');
        operand1_addr <= (others => '-');
        immediate_val <= (others => '-');
        use_immediate <= '-';
        destination_addr <= (others => '-');
        destination_enable <= '-';
        decoded_instruction_available <= '0';
      end if;
    end if;
  end process decode_sequential;

  dispatch_operand1_available <=
    'X' when TO_X01(use_immediate) = 'X' else
    '1' when use_immediate = '1' else
    dispatch_operand1_available_reg;
  dispatch_operand1_value <=
    (others => 'X') when TO_X01(use_immediate) = 'X' else
    immediate_val when use_immediate = '1' else
    dispatch_operand1_value_reg;
  dispatch_operand1_tag <=
    (others => 'X') when TO_X01(use_immediate) = 'X' else
    (others => '-') when use_immediate = '1' else
    dispatch_operand1_tag_reg;

  any_dispatch <= alu_dispatch; -- or foo_dispatch or ...

  dispatch_combinational :
  process(unit_id, unit_operation, operand0_addr, operand1_addr,
    immediate_val, use_immediate, destination_addr, destination_enable,
    decoded_instruction_available)
  begin
  end process dispatch_combinational;

  dispatch_sequential : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      
    end if;
  end process dispatch_sequential;

  reg : register_file
  port map (
    clk => clk,
    rst => rst,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    rd0_addr => operand0_addr,
    rd0_available => dispatch_operand0_available,
    rd0_value => dispatch_operand0_value,
    rd0_tag => dispatch_operand0_tag,
    rd1_addr => operand1_addr,
    rd1_available => dispatch_operand1_available_reg,
    rd1_value => dispatch_operand1_value_reg,
    rd1_tag => dispatch_operand1_tag_reg,
    wr0_addr => destination_addr,
    wr0_enable => destination_enable,
    wr0_tag => rob_bottom,
    wr1_addr_tag => rob_top,
    wr1_enable => rob_pop,
    wr1_value => rob_top_value);

  alu_dispatch <=
    alu_dispatchable when
      decoded_instruction_available = '1' and unit_id = unit_alu
    else '0';
  alu_available <= '1';

  alu_reservation_station : reservation_station
  generic map (
    unit_name => "alu",
    latency => 1,
    num_entries => 2,
    opcode_len => 3)
  port map (
    clk => clk,
    rst => rst,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation(2 downto 0),
    dispatch_operand0_available => dispatch_operand0_available,
    dispatch_operand0_value => dispatch_operand0_value,
    dispatch_operand0_tag => dispatch_operand0_tag,
    dispatch_operand1_available => dispatch_operand1_available,
    dispatch_operand1_value => dispatch_operand1_value,
    dispatch_operand1_tag => dispatch_operand1_tag,
    dispatch => alu_dispatch,
    dispatch_tag => dispatch_tag,
    dispatchable => alu_dispatchable,
    unit_available => alu_available,
    issue => alu_issue,
    issue_opcode => alu_opcode,
    issue_operand0 => alu_operand0,
    issue_operand1 => alu_operand1,
    broadcast_available => cdb_available(0),
    broadcast_tag => cdb_tag(0));

  cdb_inspect : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      for i in 0 to cdb_size-1 loop
        if TO_X01(cdb_available(i)) = 'X' then
          report "metavalue detected in cdb_available(" &
            integer'image(i) & ")"
              severity failure;
        elsif cdb_available(i) = '1' then
          assert TO_01(cdb_tag(i), 'X')(0) /= 'X'
            report "metavalue detected in cdb_tag(" &
              integer'image(i) & ")"
                severity failure;
          assert not debug_cdb
            report "CDB(" & integer'image(to_integer(cdb_tag(i))) &
              ") <- " & hex_of_word(cdb_value(i));
        end if;
      end loop;
    end if;
  end process cdb_inspect;
  mem_we <= '0';
end behavioral;
