library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
-- use work.kakeudon_fpu.all;
use work.kakeudon.all;

entity core is
  generic (
    debug_out : boolean;
    debug_out_commit : boolean;
    bypass_program_loading : boolean);
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
    -- memory refetch
    mem_refetch : out std_logic;
    -- instruction
    mem_inst_addr : out unsigned(29 downto 0);
    mem_inst_data : in unsigned(31 downto 0);
    -- Clock And Reset
    clk : in std_logic;
    rst : in std_logic);
end entity core;

architecture behavioral of core is
  constant disable_branch_prediction : boolean := false;

  signal cdb_available : std_logic_vector(0 to cdb_size-1);
  signal cdb_value : cdb_in_value_t;
  signal cdb_tag : cdb_in_tag_t;

  signal program_counter : unsigned(29 downto 0)
    := initial_program_counter(31 downto 2);

  signal refetch : std_logic;
  signal refetch_address : unsigned(31 downto 0);

  -- instruction fetch
  signal program_counter_plus1 : unsigned(29 downto 0)
    := initial_program_counter(31 downto 2);
  signal instruction_register : unsigned(31 downto 0);
  signal instruction_register_old : unsigned(31 downto 0);
  signal instruction_fetch_stalled : std_logic;
  signal instruction_predicted_branch : unsigned(29 downto 0)
    := initial_program_counter(31 downto 2);
  signal instruction_register_available : std_logic := '0';

  signal instruction_fetch_stall : std_logic := '0';

  -- instruction decode
  type unit_id_t is
    (unit_none, unit_mem, unit_branch, unit_alu,
     unit_fadd, unit_fmul, unit_fcmp, unit_fothers);
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
  signal decode_val_from_reg : std_logic;
  signal decode_val_from_reg_select : std_logic;
  signal decode_branch_from_reg : std_logic;
  signal decode_branch_available : std_logic;
  signal decode_branch_value : unsigned(31 downto 0);
  signal decode_predicted_branch : unsigned(31 downto 0);
  signal decode_program_counter_plus1 : unsigned(29 downto 0);
  signal dispatch_decode_success : std_logic;
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
  signal dispatch_program_counter_plus1 : unsigned(29 downto 0);
  signal wr0_enable : std_logic;
  signal wr0_tag : tomasulo_tag_t;

  signal none_dispatch : std_logic;
  signal dispatch_common : std_logic;
  signal any_dispatch : std_logic;

  signal mem_dispatchable : std_logic := '0';
  signal mem_dispatch : std_logic := '0';
  signal mem_issue_operand0 : unsigned(31 downto 0);
  signal ls_committable : std_logic;

  signal branch_dispatchable : std_logic;
  signal branch_dispatch : std_logic := '0';
  signal branch_available : std_logic;
  signal branch_issue : std_logic;
  signal branch_opcode : unsigned(1 downto 0);
  signal branch_operands : unsigned_word_array_t(0 to 3);

  signal alu_dispatchable : std_logic;
  signal alu_dispatch : std_logic := '0';
  signal alu_available : std_logic;
  signal alu_issue : std_logic;
  signal alu_opcode : unsigned(3 downto 0);
  signal alu_operands : unsigned_word_array_t(0 to 1);

  signal fadd_dispatchable : std_logic;
  signal fadd_dispatch : std_logic := '0';
  signal fadd_available : std_logic;
  signal fadd_issue : std_logic;
  signal fadd_opcode : unsigned(1 downto 0);
  signal fadd_operands : unsigned_word_array_t(0 to 1);

  signal fmul_dispatchable : std_logic;
  signal fmul_dispatch : std_logic := '0';
  signal fmul_available : std_logic;
  signal fmul_issue : std_logic;
  signal fmul_opcode : unsigned(1 downto 0);
  signal fmul_operands : unsigned_word_array_t(0 to 1);

  signal fothers_dispatchable : std_logic;
  signal fothers_dispatch : std_logic := '0';
  signal fothers_available : std_logic;
  signal fothers_issue : std_logic;
  signal fothers_opcode : unsigned(1 downto 0);
  signal fothers_operands : unsigned_word_array_t(0 to 1);

  signal fcmp_dispatchable : std_logic;
  signal fcmp_dispatch : std_logic := '0';
  signal fcmp_available : std_logic;
  signal fcmp_issue : std_logic;
  signal fcmp_opcode : unsigned(3 downto 0);
  signal fcmp_operands : unsigned_word_array_t(0 to 1);

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
  signal store_commit : std_logic;
  signal any_commit: std_logic;
begin
  -- mem_inst_addr <= program_counter;
  mem_inst_addr <= instruction_predicted_branch;
  instruction_register <=
    (others => 'X') when TO_X01(instruction_fetch_stalled) = 'X' else
    mem_inst_data when instruction_fetch_stalled = '0' else
    instruction_register_old;

  branch_prediction :
  process(instruction_register_available, program_counter,
          program_counter_plus1, instruction_register)
    variable bp_opcode : opcode_t;
    variable bp_funct : funct_t;
    variable bp_fmt : cop1_fmt_t;
    variable bp_imm : signed(15 downto 0);
    variable next_bp_predicted_branch : unsigned(29 downto 0);
  begin
    if instruction_register_available = '0' then
      next_bp_predicted_branch := program_counter;
    elsif TO_01(instruction_register, 'X')(0) = 'X' then
      next_bp_predicted_branch := (others => '-');
    else
      bp_opcode := to_integer(instruction_register(31 downto 26));
      bp_funct := to_integer(instruction_register(5 downto 0));
      bp_fmt := to_integer(instruction_register(25 downto 21));
      bp_imm := signed(instruction_register(15 downto 0));
      if disable_branch_prediction then
        next_bp_predicted_branch :=
          program_counter_plus1;
      elsif bp_opcode = OP_J or bp_opcode = OP_JAL then
        next_bp_predicted_branch :=
          program_counter_plus1(29 downto 26) &
          instruction_register(25 downto 0);
      elsif
          (bp_opcode = OP_BEQ or bp_opcode = OP_BNE or
           (bp_opcode = OP_COP1 and bp_fmt = COP1_FMT_BC)) and bp_imm < 0 then
        next_bp_predicted_branch :=
          program_counter_plus1(29 downto 26) &
          unsigned(
            signed(program_counter_plus1(25 downto 0)) + bp_imm);
      else
        next_bp_predicted_branch :=
          program_counter_plus1;
      end if;
    end if;
    instruction_predicted_branch <= next_bp_predicted_branch;
  end process branch_prediction;

  instruction_fetch_sequential : process(clk, rst)
  begin
    if rst = '1' then
      if bypass_program_loading then
        program_counter <= (others => '0');
      else
        program_counter <= initial_program_counter(31 downto 2);
      end if;
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
        assert not debug_out
          report "refetch; address is " & hex_of_word(refetch_address)
            severity note;
        program_counter <= refetch_address(31 downto 2);
        instruction_register_available <= '0';
        instruction_fetch_stalled <= '-';
      elsif instruction_fetch_stall /= '1' then
        if TO_01(instruction_predicted_branch, 'X')(0) = 'X' then
          assert not debug_out
            report "metavalue detected in instruction_predicted_branch"
              severity note;
          program_counter <= (others => 'X');
        else
          assert not debug_out
            report "instruction_predicted_branch = " &
              hex_of_word(instruction_predicted_branch&"00")
                severity note;
          program_counter <= instruction_predicted_branch + 1;
        end if;
        instruction_register_available <= '1';
        instruction_fetch_stalled <= '0';
      else
        instruction_fetch_stalled <= '1';
      end if;
      instruction_register_old <= instruction_register;
    end if;
  end process instruction_fetch_sequential;
  program_counter_plus1 <= program_counter;

  instruction_fetch_stall <=
    instruction_register_available and decode_stall;

  decode_sequential : process(clk, rst)
    variable d_opcode : opcode_t;
    variable d_funct : funct_t;
    constant d_zero : internal_register_t := "0000000";
    constant d_ra : internal_register_t := "0011111";
    constant d_cc0 : internal_register_t := "1000000";
    variable d_rs : internal_register_t;
    variable d_rt : internal_register_t;
    variable d_rd : internal_register_t;
    variable d_sa : unsigned(4 downto 0);
    variable d_fmt : cop1_fmt_t;
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
    variable next_destination_addr : internal_register_t;
    variable next_decode_val_from_reg : std_logic;
    variable next_decode_val_from_reg_select : std_logic;
    variable next_decode_branch_from_reg : std_logic;
    variable next_decode_branch_available : std_logic;
    variable next_decode_branch_value : unsigned(31 downto 0);
    variable next_dispatch_decode_success : std_logic;
    variable next_decoded_instruction_available : std_logic;
  begin
    if rst = '1' then
      decode_rob_type <= rob_type_calc; -- don't care
      unit_id <= unit_none; -- don't care
      unit_operation <= (others => '-');
      operand0_use_immediate <= '-';
      operand0_addr <= (others => '-');
      operand0_immediate_val <= (others => '-');
      operand1_use_immediate <= '-';
      operand1_addr <= (others => '-');
      operand1_immediate_val <= (others => '-');
      operand2_immediate_val <= (others => '-');
      operand3_immediate_val <= (others => '-');
      destination_addr <= (others => '0');
      decode_val_from_reg <= '-';
      decode_val_from_reg_select <= '-';
      decode_branch_from_reg <= '-';
      decode_branch_available <= '-';
      decode_branch_value <= (others => '-');
      decode_predicted_branch <= (others => '-');
      decode_program_counter_plus1 <= (others => '-');
      dispatch_decode_success <= '-';
      decoded_instruction_available <= '0';
    elsif rising_edge(clk) then
      if refetch = '1' then
        decode_rob_type <= rob_type_calc; -- don't care
        unit_id <= unit_none; -- don't care
        unit_operation <= (others => '-');
        operand0_use_immediate <= '-';
        operand0_addr <= (others => '-');
        operand0_immediate_val <= (others => '-');
        operand1_use_immediate <= '-';
        operand1_addr <= (others => '-');
        operand1_immediate_val <= (others => '-');
        operand2_immediate_val <= (others => '-');
        operand3_immediate_val <= (others => '-');
        destination_addr <= (others => '0');
        decode_val_from_reg <= '-';
        decode_val_from_reg_select <= '-';
        decode_branch_from_reg <= '-';
        decode_branch_available <= '-';
        decode_branch_value <= (others => '-');
        decode_predicted_branch <= (others => '-');
        decode_program_counter_plus1 <= (others => '-');
        dispatch_decode_success <= '-';
        decoded_instruction_available <= '0';
      elsif instruction_register_available = '1' and decode_stall /= '1' then
        next_decode_rob_type := rob_type_calc; -- don't care
        next_unit_id := unit_none; -- don't care
        next_unit_operation := (others => '-');
        next_operand0_use_immediate := '-';
        next_operand0_addr := (others => '-');
        next_operand0_immediate_val := (others => '-');
        next_operand1_use_immediate := '-';
        next_operand1_addr := (others => '-');
        next_operand1_immediate_val := (others => '-');
        next_operand2_immediate_val := (others => '-');
        next_operand3_immediate_val := (others => '-');
        next_destination_addr := (others => '-');
        next_decode_val_from_reg := '-';
        next_decode_val_from_reg_select := '-';
        next_decode_branch_from_reg := '-';
        next_decode_branch_available := '-';
        next_decode_branch_value := (others => '-');
        next_dispatch_decode_success := '1';
        next_decoded_instruction_available := '-';

        if TO_01(instruction_register, 'X')(0) = 'X' then
          next_dispatch_decode_success := '0';
          assert not debug_out
            report "metavalue detected in instruction_register"
              severity note;
        else
        d_opcode := to_integer(instruction_register(31 downto 26));
        d_funct := to_integer(instruction_register(5 downto 0));
        d_rs := "00" & instruction_register(25 downto 21);
        d_rt := "00" & instruction_register(20 downto 16);
        d_rd := "00" & instruction_register(15 downto 11);
        d_sa := instruction_register(10 downto 6);
        d_fmt := to_integer(instruction_register(25 downto 21));
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
          when FUNCT_SLL | FUNCT_SRL | FUNCT_SRA =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_alu;
            if d_funct = FUNCT_SLL then
              next_unit_operation :=
                to_unsigned(ALU_OP_SLL, unit_operation'length);
            elsif d_funct = FUNCT_SRL then
              next_unit_operation :=
                to_unsigned(ALU_OP_SRL, unit_operation'length);
            elsif d_funct = FUNCT_SRA then
              next_unit_operation :=
                to_unsigned(ALU_OP_SRA, unit_operation'length);
            else
              assert false severity failure;
            end if;
            next_operand0_use_immediate := '1';
            next_operand0_immediate_val := d_zero_ext_sa;
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_rt;
            next_destination_addr := d_rd;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when FUNCT_SLLV | FUNCT_SRLV | FUNCT_SRAV |
               FUNCT_ADD | FUNCT_ADDU | FUNCT_SUB | FUNCT_SUBU |
               FUNCT_AND | FUNCT_OR | FUNCT_XOR | FUNCT_NOR |
               FUNCT_SLT | FUNCT_SLTU =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_alu;
            if d_funct = FUNCT_SLLV then
              next_unit_operation :=
                to_unsigned(ALU_OP_SLL, unit_operation'length);
            elsif d_funct = FUNCT_SRLV then
              next_unit_operation :=
                to_unsigned(ALU_OP_SRL, unit_operation'length);
            elsif d_funct = FUNCT_SRAV then
              next_unit_operation :=
                to_unsigned(ALU_OP_SRA, unit_operation'length);
            elsif d_funct = FUNCT_ADD then
              next_unit_operation :=
                to_unsigned(ALU_OP_ADD, unit_operation'length);
            elsif d_funct = FUNCT_ADDU then
              next_unit_operation :=
                to_unsigned(ALU_OP_ADDU, unit_operation'length);
            elsif d_funct = FUNCT_SUB then
              next_unit_operation :=
                to_unsigned(ALU_OP_SUB, unit_operation'length);
            elsif d_funct = FUNCT_SUBU then
              next_unit_operation :=
                to_unsigned(ALU_OP_SUBU, unit_operation'length);
            elsif d_funct = FUNCT_AND then
              next_unit_operation :=
                to_unsigned(ALU_OP_AND, unit_operation'length);
            elsif d_funct = FUNCT_OR then
              next_unit_operation :=
                to_unsigned(ALU_OP_OR, unit_operation'length);
            elsif d_funct = FUNCT_XOR then
              next_unit_operation :=
                to_unsigned(ALU_OP_XOR, unit_operation'length);
            elsif d_funct = FUNCT_NOR then
              next_unit_operation :=
                to_unsigned(ALU_OP_NOR, unit_operation'length);
            elsif d_funct = FUNCT_SLT then
              next_unit_operation :=
                to_unsigned(ALU_OP_SLT, unit_operation'length);
            elsif d_funct = FUNCT_SLTU then
              next_unit_operation :=
                to_unsigned(ALU_OP_SLTU, unit_operation'length);
            else
              assert false severity failure;
            end if;
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_rs;
            next_operand0_immediate_val := (others => '-');
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_rt;
            next_operand1_immediate_val := (others => '-');
            next_destination_addr := d_rd;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when FUNCT_JR | FUNCT_JALR =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_none;
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_rs;
            next_operand1_use_immediate := '1';
            next_operand1_immediate_val := program_counter_plus1 & "00";
            if d_funct = FUNCT_JR then
              next_destination_addr := d_zero;
            elsif d_funct = FUNCT_JALR then
              next_destination_addr := d_rd;
            else
              assert false severity failure;
            end if;
            next_decode_val_from_reg := '1';
            next_decode_val_from_reg_select := '1';
            next_decode_branch_from_reg := '1';
            next_decode_branch_available := '0';
            next_decoded_instruction_available := '1';
          when others =>
            next_dispatch_decode_success := '0';
            assert not debug_out
              report "unknown SPECIAL funct " & bin_of_int(d_funct,6)
                severity note;
          end case;
        when OP_J | OP_JAL =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_none;
          next_unit_operation :=
            to_unsigned(ALU_OP_ADDU, unit_operation'length);
          next_operand1_use_immediate := '1';
          next_operand1_immediate_val := program_counter_plus1 & "00";
          if d_opcode = OP_J then
            next_destination_addr := d_zero;
          elsif d_opcode = OP_JAL then
            next_destination_addr := d_ra;
          else
            assert false severity failure;
          end if;
          next_decode_val_from_reg := '1';
          next_decode_val_from_reg_select := '1';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value :=
            program_counter_plus1(29 downto 26) &
            instruction_register(25 downto 0) & "00";
          next_decoded_instruction_available := '1';
        when OP_BEQ | OP_BNE =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_branch;
          if d_opcode = OP_BEQ then
            next_unit_operation :=
              to_unsigned(0, unit_operation'length);
          else
            next_unit_operation :=
              to_unsigned(1, unit_operation'length);
          end if;
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '0';
          next_operand1_addr := d_rt;
          next_operand2_immediate_val :=
            program_counter_plus1 & "00";
          next_operand3_immediate_val :=
            unsigned(signed(program_counter_plus1) + signed(d_short_imm))
            & "00";
          next_destination_addr := d_zero;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '0';
          next_decoded_instruction_available := '1';
        when OP_ADDI | OP_ADDIU | OP_SLTI | OP_SLTIU |
             OP_ANDI | OP_ORI | OP_XORI =>
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_alu;
          if d_opcode = OP_ADDI then
            next_unit_operation :=
              to_unsigned(ALU_OP_ADD, unit_operation'length);
          elsif d_opcode = OP_ADDIU then
            next_unit_operation :=
              to_unsigned(ALU_OP_ADDU, unit_operation'length);
          elsif d_opcode = OP_SLTI then
            next_unit_operation :=
              to_unsigned(ALU_OP_SLT, unit_operation'length);
          elsif d_opcode = OP_SLTIU then
            next_unit_operation :=
              to_unsigned(ALU_OP_SLTU, unit_operation'length);
          elsif d_opcode = OP_ANDI then
            next_unit_operation :=
              to_unsigned(ALU_OP_AND, unit_operation'length);
          elsif d_opcode = OP_ORI then
            next_unit_operation :=
              to_unsigned(ALU_OP_OR, unit_operation'length);
          elsif d_opcode = OP_XORI then
            next_unit_operation :=
              to_unsigned(ALU_OP_XOR, unit_operation'length);
          else
            assert false severity failure;
          end if;
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '1';
          if d_opcode = OP_ADDI or d_opcode = OP_ADDIU or
             d_opcode = OP_SLTI or d_opcode = OP_SLTIU then
            next_operand1_immediate_val := d_sign_ext_imm;
          elsif d_opcode = OP_ANDI or d_opcode = OP_ORI or
                d_opcode = OP_XORI then
            next_operand1_immediate_val := d_zero_ext_imm;
          else
            assert false severity failure;
          end if;
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
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when OP_COP1 =>
          case d_fmt is
          when COP1_FMT_MFC1 =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_fadd;
            next_unit_operation :=
              to_unsigned(2, next_unit_operation'length);
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_fs;
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_zero;
            next_destination_addr := d_rt;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when COP1_FMT_MTC1 =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_fadd;
            next_unit_operation :=
              to_unsigned(2, next_unit_operation'length);
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_rt;
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_zero;
            next_destination_addr := d_fs;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '1';
            next_decode_branch_value := program_counter_plus1 & "00";
            next_decoded_instruction_available := '1';
          when COP1_FMT_BC =>
            next_decode_rob_type := rob_type_calc;
            next_unit_id := unit_branch;
            if instruction_register(20 downto 16) = "00000" then
              next_unit_operation :=
                to_unsigned(0, unit_operation'length);
            elsif instruction_register(20 downto 16) = "00001" then
              next_unit_operation :=
                to_unsigned(1, unit_operation'length);
            else
              next_dispatch_decode_success := '0';
              assert not debug_out
                report "BC1: unknown condition code" severity note;
            end if;
            next_operand0_use_immediate := '0';
            next_operand0_addr := d_cc0;
            next_operand1_use_immediate := '0';
            next_operand1_addr := d_zero;
            next_operand2_immediate_val :=
              program_counter_plus1 & "00";
            next_operand3_immediate_val :=
              unsigned(signed(program_counter_plus1) + signed(d_short_imm))
              & "00";
            next_destination_addr := d_zero;
            next_decode_val_from_reg := '0';
            next_decode_branch_from_reg := '0';
            next_decode_branch_available := '0';
            next_decoded_instruction_available := '1';
          when COP1_FMT_S =>
            case d_funct is
              when
                  COP1_FUNCT_ADD | COP1_FUNCT_SUB | COP1_FUNCT_MOV |
                  COP1_FUNCT_NEG | COP1_FUNCT_MUL | COP1_FUNCT_DIV |
                  COP1_FUNCT_SQRT | COP1_FUNCT_CVT_W =>
                next_decode_rob_type := rob_type_calc;
                if d_funct = COP1_FUNCT_DIV or
                   d_funct = COP1_FUNCT_SQRT or
                   d_funct = COP1_FUNCT_CVT_W then
                  next_unit_id := unit_fothers;
                elsif d_funct = COP1_FUNCT_MUL then
                  next_unit_id := unit_fmul;
                else
                  next_unit_id := unit_fadd;
                end if;
                if d_funct = COP1_FUNCT_ADD then
                  next_unit_operation :=
                    to_unsigned(0, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_SUB then
                  next_unit_operation :=
                    to_unsigned(1, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_MOV then
                  next_unit_operation :=
                    to_unsigned(2, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_NEG then
                  next_unit_operation :=
                    to_unsigned(3, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_MUL then
                  next_unit_operation :=
                    to_unsigned(0, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_DIV then
                  next_unit_operation :=
                    to_unsigned(0, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_SQRT then
                  next_unit_operation :=
                    to_unsigned(1, next_unit_operation'length);
                elsif d_funct = COP1_FUNCT_CVT_W then
                  next_unit_operation :=
                    to_unsigned(3, next_unit_operation'length);
                else
                  assert false severity failure;
                end if;
                next_operand0_use_immediate := '0';
                next_operand0_addr := d_fs;
                next_operand1_use_immediate := '0';
                if d_funct = COP1_FUNCT_MOV or d_funct = COP1_FUNCT_NEG or
                   d_funct = COP1_FUNCT_SQRT or d_funct = COP1_FUNCT_CVT_W then
                  next_operand1_addr := d_zero;
                else
                  next_operand1_addr := d_ft;
                end if;
                next_destination_addr := d_fd;
                next_decode_val_from_reg := '0';
                next_decode_branch_from_reg := '0';
                next_decode_branch_available := '1';
                next_decode_branch_value := program_counter_plus1 & "00";
                next_decoded_instruction_available := '1';
              when COP1_FUNCT_C_F | COP1_FUNCT_C_UN | COP1_FUNCT_C_EQ |
                   COP1_FUNCT_C_UEQ | COP1_FUNCT_C_OLT | COP1_FUNCT_C_ULT |
                   COP1_FUNCT_C_OLE | COP1_FUNCT_C_ULE =>
                next_decode_rob_type := rob_type_calc;
                next_unit_id := unit_fcmp;
                next_unit_operation := instruction_register(3 downto 0);
                next_operand0_use_immediate := '0';
                next_operand0_addr := d_fs;
                next_operand0_immediate_val := (others => '-');
                next_operand1_use_immediate := '0';
                next_operand1_addr := d_ft;
                next_operand1_immediate_val := (others => '-');
                next_destination_addr := d_cc0;
                next_decode_val_from_reg := '0';
                next_decode_branch_from_reg := '0';
                next_decode_branch_available := '1';
                next_decode_branch_value := program_counter_plus1 & "00";
                next_decoded_instruction_available := '1';
            when others =>
              next_dispatch_decode_success := '0';
              assert not debug_out
                report "unknown COP1.S funct " & bin_of_int(d_funct,6)
                  severity note;
            end case;
          when COP1_FMT_W =>
            case d_funct is
              when COP1_FUNCT_CVT_S =>
                next_decode_rob_type := rob_type_calc;
                next_unit_id := unit_fothers;
                next_unit_operation :=
                  to_unsigned(2, next_unit_operation'length);
                next_operand0_use_immediate := '0';
                next_operand0_addr := d_fs;
                next_operand1_use_immediate := '0';
                next_operand1_addr := d_zero;
                next_destination_addr := d_fd;
                next_decode_val_from_reg := '0';
                next_decode_branch_from_reg := '0';
                next_decode_branch_available := '1';
                next_decode_branch_value := program_counter_plus1 & "00";
                next_decoded_instruction_available := '1';
            when others =>
              next_dispatch_decode_success := '0';
              assert not debug_out
                report "unknown COP1.W funct " & bin_of_int(d_funct,6)
                  severity note;
            end case;
          when others =>
            next_dispatch_decode_success := '0';
            assert not debug_out
              report "unknown COP1 fmt " & bin_of_int(d_fmt,5)
                severity note;
          end case;
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
          next_destination_addr := d_rt;
          next_decode_val_from_reg := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when OP_SW =>
          next_decode_rob_type := rob_type_store;
          next_unit_id := unit_mem;
          next_unit_operation :=
            to_unsigned(1, unit_operation'length);
          next_operand0_use_immediate := '0';
          next_operand0_addr := d_rs;
          next_operand1_use_immediate := '0';
          next_operand1_addr := d_rt;
          next_operand2_immediate_val := d_sign_ext_imm;
          next_destination_addr := d_zero;
          next_decode_val_from_reg := '1';
          next_decode_val_from_reg_select := '1';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        when others =>
          next_dispatch_decode_success := '0';
          assert not debug_out
            report "unknown opcode " & bin_of_int(d_opcode,6)
              severity note;
        end case;
        end if;
        if next_dispatch_decode_success /= '1' then
          next_decode_rob_type := rob_type_calc;
          next_unit_id := unit_none;
          next_operand0_use_immediate := '1';
          next_operand1_use_immediate := '1';
          next_destination_addr := d_zero;
          next_decode_val_from_reg := '1';
          next_decode_val_from_reg_select := '0';
          next_decode_branch_from_reg := '0';
          next_decode_branch_available := '1';
          next_decode_branch_value := program_counter_plus1 & "00";
          next_decoded_instruction_available := '1';
        end if;
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
        destination_addr <= next_destination_addr;
        decode_val_from_reg <= next_decode_val_from_reg;
        decode_val_from_reg_select <= next_decode_val_from_reg_select;
        decode_branch_from_reg <= next_decode_branch_from_reg;
        decode_branch_available <= next_decode_branch_available;
        decode_branch_value <= next_decode_branch_value;
        decode_predicted_branch <= instruction_predicted_branch & "00";
        decode_program_counter_plus1 <= program_counter_plus1;
        dispatch_decode_success <= next_dispatch_decode_success;
        decoded_instruction_available <= next_decoded_instruction_available;
      elsif decode_stall /= '1' then
        decode_rob_type <= rob_type_calc; -- don't care
        unit_id <= unit_none; -- don't care
        unit_operation <= (others => '-');
        operand0_use_immediate <= '-';
        operand0_addr <= (others => '-');
        operand0_immediate_val <= (others => '-');
        operand1_use_immediate <= '-';
        operand1_addr <= (others => '-');
        operand1_immediate_val <= (others => '-');
        operand2_immediate_val <= (others => '-');
        operand3_immediate_val <= (others => '-');
        destination_addr <= (others => '0');
        decode_val_from_reg <= '-';
        decode_val_from_reg_select <= '-';
        decode_branch_from_reg <= '-';
        decode_branch_available <= '-';
        decode_branch_value <= (others => '-');
        decode_predicted_branch <= (others => '-');
        decode_program_counter_plus1 <= (others => '-');
        dispatch_decode_success <= '-';
        decoded_instruction_available <= '0';
      end if;
    end if;
  end process decode_sequential;

  decode_stall <=
    decoded_instruction_available and not any_dispatch;

  dispatch_rob_val <=
    value_or_tag_select(
      decode_val_from_reg,
      value_or_tag_select(
        decode_val_from_reg_select,
        dispatch_operand1,
        dispatch_operand0
      ),
      value_or_tag_from_tag(rob_bottom));

  dispatch_branch <=
    ('X', (others => 'X'), (others => 'X'))
      when TO_X01(decode_branch_from_reg) = 'X' else
    dispatch_operand0 when decode_branch_from_reg = '1' else
    ('1', decode_branch_value, (others => '-'))
      when decode_branch_available = '1' else
    ('0', (others => '-'), rob_bottom);

  rob : reorder_buffer
  generic map (
    debug_out => debug_out,
    debug_out_commit => debug_out_commit)
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
    dispatch_program_counter_plus1 => decode_program_counter_plus1,
    dispatch_decode_success => dispatch_decode_success,
    rob_top_committable => rob_top_committable,
    rob_top => rob_top,
    rob_top_type => rob_top_type,
    rob_top_dest => rob_top_dest,
    rob_top_val => rob_top_val,
    refetch => refetch,
    refetch_address => refetch_address,
    rob_bottom => rob_bottom,
    rob_rd0_reg_tag => dispatch_operand0_reg.tag,
    rob_rd0 => dispatch_operand0_rob,
    rob_rd1_reg_tag => dispatch_operand1_reg.tag,
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

  none_dispatch <=
    dispatch_common when unit_id = unit_none else '0';

  dispatch_common <=
    decoded_instruction_available and rob_dispatchable;

  any_dispatch <=
    none_dispatch or mem_dispatch or branch_dispatch or alu_dispatch
    or fadd_dispatch or fmul_dispatch or fcmp_dispatch or fothers_dispatch;

  dispatch_sequential : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      
    end if;
  end process dispatch_sequential;

  wr0_enable <= any_dispatch and not refetch;
  wr0_tag <=
    rob_bottom when dispatch_rob_val.available = '1' else
    dispatch_rob_val.tag;

  reg : register_file
  generic map (
    debug_out => debug_out,
    debug_out_commit => debug_out_commit)
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
    wr0_tag => wr0_tag,
    wr1_addr => rob_top_dest,
    wr1_enable => rob_top_committable,
    wr1_tag => rob_top,
    wr1_value => rob_top_val.value);

  mem_dispatch <=
    dispatch_common and mem_dispatchable when unit_id = unit_mem else '0';

  ls_buffer_unit : load_store_buffer
  generic map (
    debug_out => debug_out,
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
    ls_committable => ls_committable,
    issue => mem_enable,
    issue_tag => mem_tag,
    issue_isstore => mem_isstore,
    issue_operand0 => mem_issue_operand0);
  mem_addr <= mem_issue_operand0(31 downto 2);
  mem_bytes <= "1111";
  send_data_to_memory : process(clk,rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      mem_data_write <= rob_top_val.value;
    end if;
  end process send_data_to_memory;

  recv_data_from_memory: process(clk, rst)
  begin
    if rst = '1' then
      cdb_available(0) <= '0';
      cdb_value(0) <= (others => '-');
      cdb_tag(0) <= (others => '-');
    elsif rising_edge(clk) then
      cdb_available(0) <= mem_avail_read and not refetch;
      cdb_value(0) <= mem_data_read;
      cdb_tag(0) <= mem_tag_read;
    end if;
  end process recv_data_from_memory;
  mem_refetch <= refetch;

  branch_dispatch <=
    dispatch_common and branch_dispatchable when unit_id = unit_branch
    else '0';
  branch_available <= '1';

  branch_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
    unit_name => "branch",
    latency => 1,
    num_entries => 2,
    num_operands => 4,
    opcode_len => 2)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation(1 downto 0),
    dispatch_operands => dispatch_operands_4,
    dispatch => branch_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => branch_dispatchable,
    unit_available => branch_available,
    issue => branch_issue,
    issue_opcode => branch_opcode,
    issue_operands => branch_operands,
    broadcast_available => cdb_available(1),
    broadcast_tag => cdb_tag(1));

  branch_process : process(clk, rst)
    variable compar_result : std_logic;
    variable branch_result : unsigned(31 downto 0);
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      assert TO_X01(branch_issue) /= 'X'
        report "metavalue detected in branch_issue"
          severity failure;
      branch_result := (others => '-');
      if branch_issue = '1' then
        assert TO_01(branch_operands(0), 'X')(0) /= 'X'
          report "metavalue detected in branch_operands(0)"
            severity failure;
        assert TO_01(branch_operands(1), 'X')(0) /= 'X'
          report "metavalue detected in branch_operands(1)"
            severity failure;
        if branch_operands(0) = branch_operands(1) then
          compar_result := '1';
        else
          compar_result := '0';
        end if;
        if (compar_result xor branch_opcode(0)) = '1' then
          branch_result := branch_operands(3);
        else
          branch_result := branch_operands(2);
        end if;
      end if;
      cdb_value(1) <= branch_result;
    end if;
  end process branch_process;

  alu_dispatch <=
    dispatch_common and alu_dispatchable when unit_id = unit_alu else '0';
  alu_available <= '1';

  alu_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
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
    broadcast_available => cdb_available(2),
    broadcast_tag => cdb_tag(2));

  alu_unit : alu
  generic map (
    debug_out => debug_out)
  port map (
    clk => clk,
    rst => rst,
    alu_opcode => alu_opcode,
    alu_in0 => alu_operands(0),
    alu_in1 => alu_operands(1),
    alu_out => cdb_value(2));

  fadd_dispatch <=
    dispatch_common and fadd_dispatchable when unit_id = unit_fadd else '0';
  fadd_available <= '1';

  fadd_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
    unit_name => "fadd",
    latency => 2,
    num_entries => 2,
    num_operands => 2,
    opcode_len => 2)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation(1 downto 0),
    dispatch_operands => dispatch_operands_2,
    dispatch => fadd_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => fadd_dispatchable,
    unit_available => fadd_available,
    issue => fadd_issue,
    issue_opcode => fadd_opcode,
    issue_operands => fadd_operands,
    broadcast_available => cdb_available(3),
    broadcast_tag => cdb_tag(3));

  fadd_unit : fp_adder
  generic map (
    debug_out => debug_out)
  port map (
    clk => clk,
    rst => rst,
    opcode => fadd_opcode,
    fp_in0 => fadd_operands(0),
    fp_in1 => fadd_operands(1),
    fp_out => cdb_value(3));

  fmul_dispatch <=
    dispatch_common and fmul_dispatchable when unit_id = unit_fmul else '0';
  fmul_available <= '1';

  fmul_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
    unit_name => "fmul",
    latency => 2,
    num_entries => 2,
    num_operands => 2,
    opcode_len => 2)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation(1 downto 0),
    dispatch_operands => dispatch_operands_2,
    dispatch => fmul_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => fmul_dispatchable,
    unit_available => fmul_available,
    issue => fmul_issue,
    issue_opcode => fmul_opcode,
    issue_operands => fmul_operands,
    broadcast_available => cdb_available(4),
    broadcast_tag => cdb_tag(4));

  fmul_unit : fp_multiplier
  generic map (
    debug_out => debug_out)
  port map (
    clk => clk,
    rst => rst,
    opcode => fmul_opcode,
    fp_in0 => fmul_operands(0),
    fp_in1 => fmul_operands(1),
    fp_out => cdb_value(4));

  fcmp_dispatch <=
    dispatch_common and fcmp_dispatchable when unit_id = unit_fcmp else '0';
  fcmp_available <= '1';

  fcmp_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
    unit_name => "fcmp",
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
    dispatch => fcmp_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => fcmp_dispatchable,
    unit_available => fcmp_available,
    issue => fcmp_issue,
    issue_opcode => fcmp_opcode,
    issue_operands => fcmp_operands,
    broadcast_available => cdb_available(5),
    broadcast_tag => cdb_tag(5));

  fcmp_unit : fp_comparator
  generic map (
    debug_out => debug_out)
  port map (
    clk => clk,
    rst => rst,
    opcode => fcmp_opcode,
    fp_in0 => fcmp_operands(0),
    fp_in1 => fcmp_operands(1),
    fp_out => cdb_value(5));

  fothers_dispatch <=
    dispatch_common and fothers_dispatchable when unit_id = unit_fothers
    else '0';
  fothers_available <= '1';

  fothers_reservation_station : reservation_station
  generic map (
    debug_out => debug_out,
    unit_name => "fothers",
    latency => 7,
    num_entries => 2,
    num_operands => 2,
    opcode_len => 2)
  port map (
    clk => clk,
    rst => rst,
    refetch => refetch,
    cdb_in_available => cdb_available,
    cdb_in_value => cdb_value,
    cdb_in_tag => cdb_tag,
    dispatch_opcode => unit_operation(1 downto 0),
    dispatch_operands => dispatch_operands_2,
    dispatch => fothers_dispatch,
    dispatch_tag => rob_bottom,
    dispatchable => fothers_dispatchable,
    unit_available => fothers_available,
    issue => fothers_issue,
    issue_opcode => fothers_opcode,
    issue_operands => fothers_operands,
    broadcast_available => cdb_available(6),
    broadcast_tag => cdb_tag(6));

  fothers_unit : fp_others
  generic map (
    debug_out => debug_out)
  port map (
    clk => clk,
    rst => rst,
    opcode => fothers_opcode,
    fp_in0 => fothers_operands(0),
    fp_in1 => fothers_operands(1),
    fp_out => cdb_value(6));

  calc_commit <=
    rob_top_committable when rob_top_type = rob_type_calc else '0';
  store_commit <=
    rob_top_committable and ls_committable
      when rob_top_type = rob_type_store else '0';
  any_commit <= calc_commit or store_commit;

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
          assert not debug_out
            report "CDB(id " & integer'image(i) &
                   ", tag " & integer'image(to_integer(cdb_tag(i))) &
                   ") <- " & hex_of_word(cdb_value(i))
              severity note;
        end if;
      end loop;
    end if;
  end process cdb_inspect;
end behavioral;
