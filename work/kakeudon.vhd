library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package kakeudon is
  constant clk_freq : real := 66.666e6;
  subtype unsigned_word is unsigned(31 downto 0);

  component core is
    port (
      -- Register File
      rs_addr : out unsigned(4 downto 0);
      rs_val : in unsigned(31 downto 0);
      rt_addr : out unsigned(4 downto 0);
      rt_val : in unsigned(31 downto 0);
      rd_addr : out unsigned(4 downto 0);
      rd_val : out unsigned(31 downto 0);
      gpr_we : out std_logic;
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
      -- ALU
      alu_control : out unsigned(5 downto 0);
      alu_in0 : out unsigned(31 downto 0);
      alu_in1 : out unsigned(31 downto 0);
      alu_out : in unsigned(31 downto 0);
      alu_iszero : in std_logic;
      -- Floating-Point Register File
      fs_addr : out unsigned(4 downto 0);
      fs_val : in unsigned(31 downto 0);
      ft_addr : out unsigned(4 downto 0);
      ft_val : in unsigned(31 downto 0);
      fd_addr : out unsigned(4 downto 0);
      fd_val : out unsigned(31 downto 0);
      fpr_we : out std_logic;
      -- FPU
      fpu_control : out unsigned(5 downto 0);
      fpu_in0 : out unsigned(31 downto 0);
      fpu_in1 : out unsigned(31 downto 0);
      fpu_out : in unsigned(31 downto 0);
      fpu_condition : in std_logic;
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
      gpr_rd0addr : in unsigned(4 downto 0);
      gpr_rd0val : out unsigned_word;
      gpr_rd1addr : in unsigned(4 downto 0);
      gpr_rd1val : out unsigned_word;
      gpr_wraddr : in unsigned(4 downto 0);
      gpr_wrval : in unsigned_word;
      gpr_we : in std_logic);
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
end package kakeudon;
