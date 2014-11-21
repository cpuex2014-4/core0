library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon_fpu.all;
use work.kakeudon.all;

entity core is
  port (
    -- main read/write
    mem_enable : out std_logic;
    mem_isstore : out std_logic;
    mem_addr : out unsigned(29 downto 0);
    mem_bytes : out unsigned(3 downto 0);
    mem_tag : out tomasulo_tag_t;
    mem_data_write : out unsigned(31 downto 0);
    mem_avail_read : in std_logic;
    mem_data_read : in unsigned(31 downto 0);
    mem_tag_read : in tomasulo_tag_t;
    -- instruction
    mem_inst_addr : out unsigned(29 downto 0);
    mem_inst_data : in unsigned(31 downto 0);
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

  signal program_counter : unsigned(29 downto 0)
    := initial_program_counter(31 downto 2);

  signal refetch : std_logic;
  signal refetch_address : unsigned(31 downto 0);

  -- instruction fetch
  signal program_counter_plus1 : unsigned(29 downto 0);
  signal instruction_register : unsigned(31 downto 0);
  signal instruction_predicted_branch : unsigned(29 downto 0);
  signal instruction_register_available : std_logic := '0';

  signal instruction_fetch_stall : std_logic := '0';

  -- instruction decode
  type unit_id_t is (unit_mem, unit_branch, unit_alu, unit_fadd);
  signal decode_rob_type : rob_type_t;
  signal unit_id : unit_id_t;
  signal unit_operation : unsigned(3 downto 0);
  signal operand0_use_immediate : std_logic;
  signal operand0_addr : internal_register_t;
  signal operand0_immediate_val : unsigned(31 downto 0);
  signal operand1_use_immediate : std_logic;
  signal operand1_addr : internal_register_t;
  signal operand1_immediate_val : unsigned(31 downto 0);
  signal operand2_immediate_val : unsigned(31 downto 0);
  signal operand3_immediate_val : unsigned(31 downto 0);
  signal destination_addr : internal_register_t;
  signal destination_enable : std_logic := '0';
  signal decode_val_from_reg : std_logic;
  signal decode_branch_from_reg : std_logic;
  signal decode_branch_available : std_logic;
  signal decode_branch_value : unsigned(31 downto 0);
  signal decode_predicted_branch : unsigned(31 downto 0);
  signal decoded_instruction_available : std_logic := '0';

  signal decode_stall : std_logic := '0';

  -- dispatch
  signal dispatch_operand0_reg : value_or_tag_t;
  signal dispatch_operand0 : value_or_tag_t;
  signal dispatch_operand1_reg : value_or_tag_t;
  signal dispatch_operand1 : value_or_tag_t;
  signal dispatch_operand2 : unsigned_word;
  signal dispatch_operand3 : unsigned_word;
  signal dispatch_operands_2 : value_or_tag_array_t(0 to 1);
  signal dispatch_operands_4 : value_or_tag_array_t(0 to 3);
  signal dispatch_rob_val : value_or_tag_t;
  signal dispatch_branch : value_or_tag_t;
  signal wr0_enable : std_logic;

  signal any_dispatch : std_logic;

  signal mem_dispatchable : std_logic := '0';
  signal mem_dispatch : std_logic := '0';
  signal mem_issue_operand0 : unsigned(31 downto 0);

  signal alu_dispatchable : std_logic;
  signal alu_dispatch : std_logic := '0';
  signal alu_available : std_logic;
  signal alu_issue : std_logic;
  signal alu_opcode : unsigned(3 downto 0);
  signal alu_operands : unsigned_word_array_t(0 to 1);

  -- reorder buffer
  signal rob_top_committable : std_logic;
  signal rob_top : tomasulo_tag_t;
  signal rob_top_type : rob_type_t;
  signal rob_top_dest : internal_register_t;
  signal rob_top_val : value_or_tag_t;
  signal rob_bottom : tomasulo_tag_t;
  signal rob_dispatchable : std_logic;
  signal dispatch_operand0_rob : value_or_tag_t;
  signal dispatch_operand1_rob : value_or_tag_t;

  -- commit
  signal calc_commit : std_logic;
  signal any_commit: std_logic;
begin
  mem_inst_addr <= program_counter;
  instruction_register <= mem_inst_data;

  instruction_fetch_sequential : process(clk, rst)
  begin
    if rst = '1' then
      program_counter <= initial_program_counter(31 downto 2);
      program_counter_plus1 <= (others => '-');
      instruction_predicted_branch <= (others => '-');
      instruction_register_available <= '0';
    elsif rising_edge(clk) then
      assert TO_X01(refetch) /= 'X'
        report "metavalue detected in refetch"
          severity failure;
      if refetch = '1' then
        assert TO_01(refetch_address(1 downto 0), 'X')(0) /= 'X'
          report "metavalue detected in refetch_address"
            severity failure;
        assert refetch_address(1 downto 0) = "00"
          report "refetch_address(1 downto 0) is not 00"
            severity failure;
        assert not debug_instruction_fetch
          report "refetch; address is " & hex_of_word(refetch_address)
            severity note;
        program_counter <= refetch_address(31 downto 2);
        program_counter_plus1 <= (others => '-');
        instruction_predicted_branch <= (others => '-');
        instruction_register_available <= '0';
      elsif instruction_fetch_stall /= '1' then
        assert TO_01(program_counter, 'X')(0) /= 'X'
          report "metavalue detected in program_counter"
            severity failure;
        assert not debug_instruction_fetch
          report "program_counter = " & hex_of_word(program_counter&"00")
            severity note;
        program_counter <= program_counter + 1;
        program_counter_plus1 <= program_counter + 1;
        instruction_predicted_branch <= program_counter + 1;
        instruction_register_available <= '1';
      end if;
    end if;
  end process instruction_fetch_sequential;

  instruction_fetch_stall <=
    instruction_register_available and decode_stall;

  decode_sequential : process(clk, rst)
    variable d_opcode : opcode_t;
    variable d_funct : funct_t;
    constant d_zero : internal_register_t := "0000000";
    constant d_ra : internal_register_t := "0011111";
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
    variable d_zero_ext_sa : unsigned(31 downto 0);

    variable next_decode_rob_type : rob_type_t;
    variable next_unit_id : unit_id_t;
    variable next_unit_operation : unsigned(3 downto 0);
    variable next_operand0_use_immediate : std_logic;
    variable next_operand0_addr : internal_register_t;
    variable next_operand0_immediate_val : unsigned(31 downto 0);
    variable next_operand1_use_immediate : std_logic;
    variable next_operand1_addr : internal_register_t;
    variable next_operand1_immediate_val : unsigned(31 downto 0);
    variable next_operand2_immediate_val : unsigned(31 downto 0);
    variable next_operand3_immediate_val : unsigned(31 downto 0);
    variable next_destination_enable : std_logic;
    variable next_destination_addr : internal_register_t;
    variable next_decode_val_from_reg : std_logic;
    variable next_decode_branch_from_reg : std_logic;
    variable next_decode_branch_available : std_logic;
    variable next_decode_branch_value : unsigned(31 downto 0);
    variable next_decoded_instruction_available : std_logic;
  begin
    if rst = '1' then
      decode_rob_type <= rob_type_calc; -- don't care
      unit_id <= unit_alu; -- don't care
      unit_operation <= (others => '-');
      operand0_use_immediate <= '-';
      operand0_addr <= (others => '-');
      operand0_immediate_val <= (others => '-');
      operand1_use_immediate <= '-';
      operand1_addr <= (others => '-');
      operand1_immediate_val <= (others => '-');
      operand2_immediate_val <= (others => '-');
      operand3_immediate_val <= (others => '-');
      destination_enable <= '0';
      destination_addr <= (others => '-');
      decode_val_from_reg <= '-';
      decode_branch_from_reg <= '-';
      decode_branch_available <= '-';
      decode_branch_value <= (others => '-');
      decode_predicted_branch <= (others => '-');
      decoded_instruction_available <= '0';
    elsif rising_edge(clk) then
      if refetch = '1' then
        decode_rob_type <= rob_type_calc; -- don't care
        unit_id <= unit_alu; -- don't care
        unit_operation <= (others => '-');
        operand0_use_immediate <= '-';
        operand0_addr <= (others => '-');
        operand0_immediate_val <= (others => '-');
        operand1_use_immediate <= '-';
        operand1_addr <= (others => '-');
        operand1_immediate_val <= (others => '-');
        operand2_immediate_val <= (others => '-');
        operand3_immediate_val <= (others => '-');
        destination_enable <= '0';
        destination_addr <= (others => '-');
        decode_val_from_reg <= '-';
        decode_branch_from_reg <= '-';
        decode_branch_available <= '-';
        decode_branch_value <= (others => '-');
        decode_predicted_branch <= (others => '-');
        decoded_instruction_available <= '0';
      elsif instruction_register_available = '1' and decode_stall /= '1' then
        next_decode_rob_type := rob_type_calc; -- don't care
        next_unit_id := unit_alu; -- don't care
        next_unit_operation := (others => '-');
        next_operand0_use_immediate := '-';
        next_operand0_addr := (others => '-');
        next_operand0_immediate_val := (others => '-');
        next_operand1_use_immediate := '-';
        next_operand1_addr := (others => '-');
        next_operand1_immediate_val := (others => '-');
        next_operand2_immediate_val := (others => '-');
        next_operand3_immediate_val := (others => '-');
        next_destination_enable := '-';
        next_destination_addr := (others => '-');
        next_decode_val_from_reg := '-';
        next_decode_branch_from_reg := '-';
        next_decode_branch_available := '-';
        next_decode_branch_value := (others => '-');
        next_decoded_instruction_available := '-';

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
        d_zero_ext_sa := resize(d_sa, 32);
        case d_opcode is
        when OP_SPECIAL =>
          case d_funct is
          when FUNCT_SLL =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_alu;
            next_unit_operation :=
              to_unsigned(ALU_OP_SLL, unit_operation'length);
            next_operand0_use_immediate := '1';
            next_operand0_immediate_val := d_zero_ext_sa;
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_rt;
            next_destination_enable := '1';
            next_destination_addr := d_rd;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when FUNCT_OR =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_alu;
            next_unit_operation := to_unsigned(ALU_OP_OR, unit_operation'length);
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_rs;
            next_operand0_immediate_val := (others => '-');
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_rt;
            next_operand1_immediate_val := (others => '-');
            next_destination_enable := '1';
            next_destination_addr := d_rd;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when others =>
            report "unknown SPECIAL funct " & bin_of_int(d_funct,6)
              severity failure;
          end case;
        when OP_J =>
          next_decode_rob_type := rob_type_branch;
          next_unit_id := unit_alu;
          next_unit_operation :=
            to_unsigned(ALU_OP_ADDU, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_zero;
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := program_counter_plus1 & "00";
          next_destination_enable := '0';
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value :=
            program_counter_plus1(29 downto 26) &
            instruction_register(25 downto 0) & "00";
          next_decoded_instruction_available := '1';
        when OP_JAL =>
          next_decode_rob_type := rob_type_branch;
          next_unit_id := unit_alu;
          next_unit_operation :=
            to_unsigned(ALU_OP_ADDU, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_zero;
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := program_counter_plus1 & "00";
          next_destination_enable := '1';
          next_destination_addr := d_ra;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value :=
            program_counter_plus1(29 downto 26) &
            instruction_register(25 downto 0) & "00";
          next_decoded_instruction_available := '1';
        when OP_BEQ =>
          next_decode_rob_type := rob_type_branch;
          next_unit_id := unit_branch;
          next_unit_operation :=
            to_unsigned(0, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '0';
          next_operand1_addr := d_rt;
          next_operand2_immediate_val :=
            program_counter_plus1 & "00";
          next_operand3_immediate_val :=
            unsigned(signed(program_counter_plus1) + signed(d_short_imm))
            & "00";
          next_destination_enable := '0';
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '0';
          next_decoded_instruction_available := '1';
        when OP_ADDIU =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_alu;
          next_unit_operation :=
            to_unsigned(ALU_OP_ADDU, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := d_sign_ext_imm;
          next_destination_enable := '1';
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when OP_ANDI =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_alu;
          next_unit_operation :=
            to_unsigned(ALU_OP_AND, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := d_sign_ext_imm;
          next_destination_enable := '1';
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when OP_LUI =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_alu;
          next_unit_operation :=
            to_unsigned(ALU_OP_SLL, unit_operation'length);
          next_operand0_use_immediate := '1';
          next_operand0_immediate_val :=
            to_unsigned(16, operand0_immediate_val'length);
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := d_zero_ext_imm;
          next_destination_enable := '1';
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when OP_LW =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_mem;
          next_unit_operation :=
            to_unsigned(0, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := (others => '-');
          next_operand2_immediate_val := d_sign_ext_imm;
          next_destination_enable := '1';
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when others =>
          report "unknown opcode " & bin_of_int(d_opcode,6)
            severity failure;
        end case;
        decode_rob_type <= next_decode_rob_type;
        unit_id <= next_unit_id;
        unit_operation <= next_unit_operation;
        operand0_use_immediate <= next_operand0_use_immediate;
        operand0_addr <= next_operand0_addr;
        operand0_immediate_val <= next_operand0_immediate_val;
        operand1_use_immediate <= next_operand1_use_immediate;
        operand1_addr <= next_operand1_addr;
        operand1_immediate_val <= next_operand1_immediate_val;
        operand2_immediate_val <= next_operand2_immediate_val;
        operand3_immediate_val <= next_operand3_immediate_val;
        destination_enable <= next_destination_enable;
        destination_addr <= next_destination_addr;
        decode_val_from_reg <= next_decode_val_from_reg;
        decode_branch_from_reg <= next_decode_branch_from_reg;
        decode_branch_available <= next_decode_branch_available;
        decode_branch_value <= next_decode_branch_value;
        decode_predicted_branch <= instruction_predicted_branch & "00";
        decoded_instruction_available <= next_decoded_instruction_available;
      elsif decode_stall /= '1' then
        decode_rob_type <= rob_type_calc; -- don't care
        unit_id <= unit_alu; -- don't care
        unit_operation <= (others => '-');
        operand0_use_immediate <= '-';
        operand0_addr <= (others => '-');
        operand0_immediate_val <= (others => '-');
        operand1_use_immediate <= '-';
        operand1_addr <= (others => '-');
        operand1_immediate_val <= (others => '-');
        operand2_immediate_val <= (others => '-');
        operand3_immediate_val <= (others => '-');
        destination_enable <= '0';
        destination_addr <= (others => '-');
        decode_val_from_reg <= '-';
        decode_branch_from_reg <= '-';
        decode_branch_available <= '-';
        decode_branch_value <= (others => '-');
        decode_predicted_branch <= (others => '-');
        decoded_instruction_available <= '0';
      end if;
    end if;
  end process decode_sequential;

  decode_stall <=
    decoded_instruction_available and not any_dispatch;

  dispatch_rob_val <=
    value_or_tag_select(
      decode_val_from_reg,
      dispatch_operand1_reg,
      ('0', (others => '-'), rob_bottom));

  dispatch_branch <=
    ('X', (others => 'X'), (others => 'X'))
      when TO_X01(decode_branch_from_reg) = 'X' else
    dispatch_operand0_reg when decode_branch_from_reg = '1' else
    ('1', decode_branch_value, (others => '-'))
      when decode_branch_available = '1' else
    ('0', (others => '-'), rob_bottom);

  rob : reorder_buffer
  port map (
    clk => clk,
    rst => rst,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatchable => rob_dispatchable,
    dispatch => any_dispatch,
    dispatch_type => decode_rob_type,
    dispatch_dest => destination_addr,
    dispatch_rob_val => dispatch_rob_val,
    dispatch_branch => dispatch_branch,
    dispatch_predicted_branch => decode_predicted_branch,
    rob_top_committable => rob_top_committable,
    rob_top => rob_top,
    rob_top_type => rob_top_type,
    rob_top_dest => rob_top_dest,
    rob_top_val => rob_top_val,
    refetch => refetch,
    refetch_address => refetch_address,
    rob_bottom => rob_bottom,
    rob_rd0_tag => dispatch_operand0_reg.tag,
    rob_rd0 => dispatch_operand0_rob,
    rob_rd1_tag => dispatch_operand1_reg.tag,
    rob_rd1 => dispatch_operand1_rob,
    commit => any_commit);

  dispatch_operand0 <=
    value_or_tag_merge(
      value_or_tag_select(
        operand0_use_immediate,
        value_or_tag_from_value(operand0_immediate_val),
        dispatch_operand0_reg),
      dispatch_operand0_rob.available,
      dispatch_operand0_rob.value);
  dispatch_operand1 <=
    value_or_tag_merge(
      value_or_tag_select(
        operand1_use_immediate,
        value_or_tag_from_value(operand1_immediate_val),
        dispatch_operand1_reg),
      dispatch_operand1_rob.available,
      dispatch_operand1_rob.value);

  dispatch_operand2 <= operand2_immediate_val;

  dispatch_operand3 <= operand3_immediate_val;

  dispatch_operands_2 <= (dispatch_operand0, dispatch_operand1);
  dispatch_operands_4 <= (
    dispatch_operand0,
    dispatch_operand1,
    value_or_tag_from_value(dispatch_operand2),
    value_or_tag_from_value(dispatch_operand3)
  );

  any_dispatch <= mem_dispatch or alu_dispatch;

  dispatch_combinational :
  process(unit_id, unit_operation, operand0_addr, operand1_addr,
    operand0_immediate_val, operand0_use_immediate,
    operand1_immediate_val, operand1_use_immediate,
    destination_addr, destination_enable,
    decoded_instruction_available)
  begin
  end process dispatch_combinational;

  dispatch_sequential : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      
    end if;
  end process dispatch_sequential;

  wr0_enable <= any_dispatch and destination_enable;

  reg : register_file
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    rd0_addr => operand0_addr,
    rd0 => dispatch_operand0_reg,
    rd1_addr => operand1_addr,
    rd1 => dispatch_operand1_reg,
    wr0_addr => destination_addr,
    wr0_enable => wr0_enable,
    wr0_tag => rob_bottom,
    wr1_addr => rob_top_dest,
    wr1_enable => rob_top_committable,
    wr1_value => rob_top_val.value);

  mem_dispatch <=
    mem_dispatchable when
      decoded_instruction_available = '1' and unit_id = unit_mem
    else '0';

  ls_buffer_unit : load_store_buffer
  generic map (
    num_stage1_entries => 2,
    num_stage2_entries => 2)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_isstore => unit_operation(0),
    dispatch_operand0 => dispatch_operand0,
    dispatch_operand2 => dispatch_operand2,
    dispatch => mem_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => mem_dispatchable,
    rob_top_committable => rob_top_committable,
    rob_top => rob_top,
    ls_committable => open, -- TODO
    issue => mem_enable,
    issue_tag => mem_tag,
    issue_isstore => mem_isstore,
    issue_operand0 => mem_issue_operand0);
  mem_addr <= mem_issue_operand0(31 downto 2);
  mem_bytes <= "1111";

  cdb_available(1) <= mem_avail_read;
  cdb_value(1) <= mem_data_read;
  cdb_tag(1) <= mem_tag_read;

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
    num_operands => 2,
    opcode_len => 4)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation,
    dispatch_operands => dispatch_operands_2,
    dispatch => alu_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => alu_dispatchable,
    unit_available => alu_available,
    issue => alu_issue,
    issue_opcode => alu_opcode,
    issue_operands => alu_operands,
    broadcast_available => cdb_available(0),
    broadcast_tag => cdb_tag(0));

  alu_unit : alu
  port map (
    alu_opcode => alu_opcode,
    alu_in0 => alu_operands(0),
    alu_in1 => alu_operands(1),
    alu_out => cdb_value(0));

  calc_commit <= rob_top_committable when rob_top_type = rob_type_calc else '0';
  any_commit <= calc_commit;

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
            report "CDB(id " & integer'image(i) &
                   ", tag " & integer'image(to_integer(cdb_tag(i))) &
                   ") <- " & hex_of_word(cdb_value(i))
              severity note;
        end if;
      end loop;
    end if;
  end process cdb_inspect;
end behavioral;
