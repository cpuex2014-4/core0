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
      alu_control : out unsigned(4 downto 0);
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
      alu_control : in unsigned(4 downto 0);
      alu_in0 : in unsigned(31 downto 0);
      alu_in1 : in unsigned(31 downto 0);
      alu_out : buffer unsigned(31 downto 0);
      alu_iszero : out std_logic);
  end component alu;

  component fpu_stub is
    port (
      clk : in std_logic;
      fpu_control : in unsigned(5 downto 0);
      fpu_in0 : in unsigned(31 downto 0);
      fpu_in1 : in unsigned(31 downto 0);
      fpu_out : out unsigned(31 downto 0);
      fpu_condition : out std_logic);
  end component fpu_stub;

  subtype opcode_t is integer range 0 to 63;
  constant OP_SPECIAL : opcode_t := 0;
  constant OP_J : opcode_t := 2;
  constant OP_JAL : opcode_t := 3;
  constant OP_BEQ : opcode_t := 4;
  constant OP_ADDI : opcode_t := 8;
  constant OP_ADDIU : opcode_t := 9;
  constant OP_SLTI : opcode_t := 10;
  constant OP_SLTIU : opcode_t := 11;
  constant OP_ANDI : opcode_t := 12;
  constant OP_ORI : opcode_t := 13;
  constant OP_XORI : opcode_t := 14;
  constant OP_LUI : opcode_t := 15;
  constant OP_COP1 : opcode_t := 17;
  constant OP_LW : opcode_t := 35;
  constant OP_SW : opcode_t := 43;
  constant OP_RRB : opcode_t := 28;
  constant OP_RSB : opcode_t := 29;
end package kakeudon;
