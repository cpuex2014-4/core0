library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package kakeudon is
  constant clk_freq : real := 66.666e6;
  subtype unsigned_word is unsigned(31 downto 0);

  subtype internal_register_t is unsigned(6 downto 0);
  subtype tomasulo_tag_t is unsigned(3 downto 0);

  constant cdb_size : natural := 1;
  subtype cdb_id_t is integer range 0 to cdb_size-1;
  subtype cdb_extended_id_t is integer range 0 to cdb_size;

  type cdb_in_value_t is array(0 to cdb_size-1) of unsigned_word;
  type cdb_in_tag_t is array(0 to cdb_size-1) of tomasulo_tag_t;

  attribute ram_style : string;

  component reservation_station is
    generic (
      unit_name : string;
      latency : natural;
      num_entries : natural;
      opcode_len : natural);
    port (
      clk : in std_logic;
      rst : in std_logic;
      cdb_in_available : in std_logic_vector(0 to cdb_size-1);
      cdb_in_value : in cdb_in_value_t;
      cdb_in_tag : in cdb_in_tag_t;
      dispatch_opcode : in unsigned(opcode_len-1 downto 0);
      dispatch_operand0_available : in std_logic;
      dispatch_operand0_value : in unsigned_word;
      dispatch_operand0_tag : in tomasulo_tag_t;
      dispatch_operand1_available : in std_logic;
      dispatch_operand1_value : in unsigned_word;
      dispatch_operand1_tag : in tomasulo_tag_t;
      dispatch : in std_logic;
      dispatch_tag : in tomasulo_tag_t;
      dispatchable : out std_logic := '1';
      unit_available : in std_logic;
      issue : out std_logic := '0';
      issue_opcode : out unsigned(opcode_len-1 downto 0);
      issue_operand0 : out unsigned_word;
      issue_operand1 : out unsigned_word;
      broadcast_available : out std_logic;
      broadcast_tag : out tomasulo_tag_t);
  end component reservation_station;

  component core is
    port (
      -- Memory Controller
      mem_addr : out unsigned(29 downto 0);
      mem_data_write : out unsigned(31 downto 0);
      mem_data_read : in unsigned(31 downto 0);
      mem_we : out std_logic;
      -- RS-232C I/O Controller
      rs232c_recv_empty : in std_logic;
      rs232c_recv_top : in unsigned(7 downto 0);
      rs232c_recv_consume : out std_logic;
      rs232c_send_full : in std_logic;
      rs232c_send_bottom : out unsigned(7 downto 0);
      rs232c_send_push : out std_logic;
      -- Clock And Reset
      clk : in std_logic;
      rst : in std_logic);
  end component core;
  component cpu is
    port (
      -- SRAM
      ZD : inout std_logic_vector(31 downto 0); -- SRAM Data
      ZDP : inout std_logic_vector(3 downto 0); -- SRAM Data, Parity
      ZA : out std_logic_vector(19 downto 0); -- SRAM Address
      XE1, E2A, XE3 : out std_logic; -- SRAM Chip Enables
      XZBE : out std_logic_vector(3 downto 0); -- SRAM Byte Enables
      XGA : out std_logic; -- SRAM Output Enable
      XWA : out std_logic; -- SRAM Write Enable
      XZCKE : out std_logic; -- SRAM Clock Enable
      ZCLKMA : out std_logic_vector(1 downto 0); -- SRAM Clock
      ADVA : out std_logic; -- SRAM Burst Mode / Negative Load Address
      XFT : out std_logic; -- SRAM Flow Through Mode
      XLBO : out std_logic; -- SRAM Linear Burst Order
      ZZA : out std_logic; -- SRAM Sleep Mode
      -- Serial I/O
      RS_TX : out std_logic; -- RS-232C, output
      RS_RX : in std_logic; -- RS-232C, input
      -- Clock And Reset
      clk : in std_logic;
      rst : in std_logic);
  end component cpu;

  component register_file is
    port (
      clk : in std_logic;
      rst : in std_logic;
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
      wr1_addr_tag : in tomasulo_tag_t;
      wr1_enable : in std_logic;
      wr1_value : in unsigned_word);
  end component register_file;

  component fp_register_file is
    port (
      clk : in std_logic;
      rst : in std_logic;
      fpr_rd0addr : in unsigned(4 downto 0);
      fpr_rd0val : out unsigned_word;
      fpr_rd1addr : in unsigned(4 downto 0);
      fpr_rd1val : out unsigned_word;
      fpr_wraddr : in unsigned(4 downto 0);
      fpr_wrval : in unsigned_word;
      fpr_we : in std_logic);
  end component fp_register_file;

  component memory_controller is
    port (
      clk : in std_logic;
      addr : in unsigned(29 downto 0);
      data_write : in unsigned(31 downto 0);
      data_read : out unsigned(31 downto 0);
      we : in std_logic;
      -- SRAM
      ZD : inout std_logic_vector(31 downto 0); -- SRAM Data
      ZDP : inout std_logic_vector(3 downto 0); -- SRAM Data, Parity
      ZA : out std_logic_vector(19 downto 0); -- SRAM Address
      XE1, E2A, XE3 : out std_logic; -- SRAM Chip Enables
      XZBE : out std_logic_vector(3 downto 0); -- SRAM Byte Enables
      XGA : out std_logic; -- SRAM Output Enable
      XWA : out std_logic; -- SRAM Write Enable
      XZCKE : out std_logic; -- SRAM Clock Enable
      ZCLKMA : out std_logic_vector(1 downto 0); -- SRAM Clock
      ADVA : out std_logic; -- SRAM Burst Mode / Negative Load Address
      XFT : out std_logic; -- SRAM Flow Through Mode
      XLBO : out std_logic; -- SRAM Linear Burst Order
      ZZA : out std_logic); -- SRAM Sleep Mode
  end component memory_controller;

  component io_rs232c is
    port (
      clk : in std_logic;
      rst : in std_logic;
      RS_RX : in std_logic;
      RS_TX : out std_logic;
      recv_empty : out std_logic;
      recv_top : out unsigned(7 downto 0);
      recv_consume : in std_logic;
      send_full : out std_logic;
      send_bottom : in unsigned(7 downto 0);
      send_push : in std_logic);
  end component io_rs232c;

  component alu is
    port (
      alu_control : in unsigned(5 downto 0);
      alu_in0 : in unsigned(31 downto 0);
      alu_in1 : in unsigned(31 downto 0);
      alu_out : buffer unsigned(31 downto 0);
      alu_iszero : out std_logic);
  end component alu;

  subtype opcode_t is integer range 0 to 63;
  constant OP_SPECIAL : opcode_t := 2#000000#;
  constant OP_J       : opcode_t := 2#000010#;
  constant OP_JAL     : opcode_t := 2#000011#;
  constant OP_BEQ     : opcode_t := 2#000100#;
  constant OP_BNE     : opcode_t := 2#000101#;
  constant OP_ADDI    : opcode_t := 2#001000#;
  constant OP_ADDIU   : opcode_t := 2#001001#;
  constant OP_SLTI    : opcode_t := 2#001010#;
  constant OP_SLTIU   : opcode_t := 2#001011#;
  constant OP_ANDI    : opcode_t := 2#001100#;
  constant OP_ORI     : opcode_t := 2#001101#;
  constant OP_XORI    : opcode_t := 2#001110#;
  constant OP_LUI     : opcode_t := 2#001111#;
  constant OP_COP1    : opcode_t := 2#010001#;
  constant OP_LW      : opcode_t := 2#100011#;
  constant OP_SW      : opcode_t := 2#101011#;
  constant OP_RRB     : opcode_t := 2#011100#;
  constant OP_RSB     : opcode_t := 2#011101#;

  subtype funct_t is integer range 0 to 63;
  constant FUNCT_SLL  : funct_t := 2#000000#;
  constant FUNCT_SRL  : funct_t := 2#000010#;
  constant FUNCT_SRA  : funct_t := 2#000011#;
  constant FUNCT_SLLV : funct_t := 2#000100#;
  constant FUNCT_SRLV : funct_t := 2#000110#;
  constant FUNCT_SRAV : funct_t := 2#000111#;
  constant FUNCT_JR   : funct_t := 2#001000#;
  constant FUNCT_JALR : funct_t := 2#001001#;
  constant FUNCT_ADD  : funct_t := 2#100000#;
  constant FUNCT_ADDU : funct_t := 2#100001#;
  constant FUNCT_SUB  : funct_t := 2#100010#;
  constant FUNCT_SUBU : funct_t := 2#100011#;
  constant FUNCT_AND  : funct_t := 2#100100#;
  constant FUNCT_OR   : funct_t := 2#100101#;
  constant FUNCT_XOR  : funct_t := 2#100110#;
  constant FUNCT_NOR  : funct_t := 2#100111#;
  constant FUNCT_SLT  : funct_t := 2#101010#;
  constant FUNCT_SLTU : funct_t := 2#101011#;

  subtype cop1_fmt_t is integer range 0 to 31;
  constant COP1_FMT_MFC1 : cop1_fmt_t := 2#00000#;
  constant COP1_FMT_MTC1 : cop1_fmt_t := 2#00100#;
  constant COP1_FMT_BC   : cop1_fmt_t := 2#01000#;
  constant COP1_FMT_S    : cop1_fmt_t := 2#10000#;
  constant COP1_FMT_D    : cop1_fmt_t := 2#10001#;
  constant COP1_FMT_W    : cop1_fmt_t := 2#10100#;
  constant COP1_FMT_L    : cop1_fmt_t := 2#10101#;

  subtype cop1_funct_t is integer range 0 to 63;
  constant COP1_FUNCT_ADD   : cop1_funct_t := 2#000000#;
  constant COP1_FUNCT_SUB   : cop1_funct_t := 2#000001#;
  constant COP1_FUNCT_MUL   : cop1_funct_t := 2#000010#;
  constant COP1_FUNCT_DIV   : cop1_funct_t := 2#000011#;
  constant COP1_FUNCT_MOV   : cop1_funct_t := 2#000110#;
  constant COP1_FUNCT_CVT_S : cop1_funct_t := 2#100000#;
  constant COP1_FUNCT_CVT_W : cop1_funct_t := 2#100100#;
  constant COP1_FUNCT_C_EQ  : cop1_funct_t := 2#110010#;
  constant COP1_FUNCT_C_OLT : cop1_funct_t := 2#110100#;
  constant COP1_FUNCT_C_OLE : cop1_funct_t := 2#110110#;

  function name_of_internal_register(r:internal_register_t) return string;

  function bin_of_int(i:natural; l:natural) return string;
  function hex_of_word(u:unsigned(31 downto 0)) return string;
  function str_of_float(f:unsigned(31 downto 0)) return string;

  type instruction_rom_t is
    array(0 to 31) of unsigned(31 downto 0);
  -- mapped to 0xBFC00000 (0x1FC00000)
  constant instruction_rom_data : instruction_rom_t := (
  -- loader: (0xBFC00000)
    x"24100000", -- li $s0, 0
  -- loader_loop: (0xBFC00004)
    x"24110000", -- li $s1, 0
    x"0ff00017", -- jal loader_recv_byte
    x"00021600", -- sll $v0, $v0, 24
    x"02228825", -- or $s1, $s1, $v0
    x"0ff00017", -- jal loader_recv_byte
    x"00021400", -- sll $v0, $v0, 16
    x"02228825", -- or $s1, $s1, $v0
    x"0ff00017", -- jal loader_recv_byte
    x"00021200", -- sll $v0, $v0, 8
    x"02228825", -- or $s1, $s1, $v0
    x"0ff00017", -- jal loader_recv_byte
    x"02228825", -- or $s1, $s1, $v0
    x"ae110000", -- sw $s1, 0($s0)
    x"26100004", -- addiu $s0, $s0, 4
    x"26310001", -- addiu $s1, $s1, 1
    x"1620fff0", -- bne $s1, $zero, loader_loop
    x"24080000", -- li $t0, 0
    x"24020000", -- li $v0, 0
    x"24100000", -- li $s0, 0
    x"24110000", -- li $s1, 0
    x"241f0000", -- li $ra, 0
    x"00000008", -- jr $zero
  -- loader_recv_byte: (0xBFC0005C)
    x"3c08ffff", -- li $t0, 0xffff0000
  -- rd_poll: (0xBFC00060)
    x"8d020000", -- lw $v0, 0($t0)
    x"30420001", -- andi $v0, $v0, 0x01
    x"1040fffd", -- beq $v0, $zero, rd_poll
    x"8d020004", -- lw $v0, 4($t0)
    x"304200ff", -- andi $v0, $v0, 0xff
    x"03e00008",  -- jr $ra
    others => (others => '0')
  );
end package kakeudon;

package body kakeudon is
  function name_of_internal_register(r:internal_register_t) return string is
    type gpr_name_t is array(0 to 31) of string(1 to 3);
    constant gpr_name : gpr_name_t := (
    "$ze", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
    "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
    "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
    "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra");
  begin
    if TO_01(r, 'X')(0) = 'X' then
      return "$UNKNOWN";
    elsif r = 0 then
      return "$zero";
    elsif r < 32 then
      return gpr_name(to_integer(r));
    elsif r < 64 then
      return "$f" & integer'image(to_integer(r-32));
    else
      return "$" & integer'image(to_integer(r));
    end if;
  end function name_of_internal_register;
  function bin_of_int(i:natural; l:natural) return string is
  begin
    if l <= 0 then
      return "";
    else
      return bin_of_int(i / 2, l - 1) & integer'image(i - i / 2 * 2);
    end if;
  end function bin_of_int;
  function hex_of_word(u:unsigned(31 downto 0)) return string is
    type hex_table_t is array(0 to 15) of string(1 to 1);
    constant hex_table : hex_table_t := (
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
      "A", "B", "C", "D", "E", "F");
  begin
    if TO_01(u, 'X')(0) = 'X' then
      return "0xXXXXXXXX";
    else
      return "0x" &
        hex_table(to_integer(u(31 downto 28))) &
        hex_table(to_integer(u(27 downto 24))) &
        hex_table(to_integer(u(23 downto 20))) &
        hex_table(to_integer(u(19 downto 16))) &
        hex_table(to_integer(u(15 downto 12))) &
        hex_table(to_integer(u(11 downto  8))) &
        hex_table(to_integer(u( 7 downto  4))) &
        hex_table(to_integer(u( 3 downto  0)));
    end if;
  end function hex_of_word;

  function str_of_float(f:unsigned(31 downto 0)) return string is
    variable f_exp : integer;
    variable f_coef : integer;
    variable f_sgn_factor : integer;
    variable ff : real;
  begin
    f_exp := to_integer(f(30 downto 23));
    f_coef := to_integer(f(22 downto 0));
    if f_exp = 255 then
      if f_coef = 0 then
        return "Inf";
      else
        return "NaN";
      end if;
    end if;
    if f(31) = '1' then
      f_sgn_factor := -1;
    else
      f_sgn_factor := 1;
    end if;
    if f_exp = 0 then
      ff := real(f_sgn_factor * f_coef) * (2 ** real(-23-126));
    else
      ff := (real(f_sgn_factor * f_coef) * (2 ** real(-23)) + 1.0)
               * (2 ** real(f_exp-127));
    end if;
    return real'image(ff);
  end function str_of_float;
end package body kakeudon;
