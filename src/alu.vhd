library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity alu is
  generic (
    debug_out : boolean;
    last_unit : boolean);
  port (
    clk : in std_logic;
    rst : in std_logic;
    refetch : in std_logic;
    alu_in_available : in std_logic;
    alu_in_tag : in tomasulo_tag_t;
    alu_opcode : in unsigned(3 downto 0);
    alu_in0 : in unsigned(31 downto 0);
    alu_in1 : in unsigned(31 downto 0);
    alu_out_available : out std_logic;
    alu_out_value : out unsigned(31 downto 0);
    alu_out_tag : out tomasulo_tag_t;
    cdb_writable : in std_logic;
    cdb_writable_next : out std_logic;
    alu_unit_available : out std_logic);
end entity alu;

architecture behavioral of alu is
  signal alu_available1 : std_logic := '0';
  signal alu_value1 : unsigned(31 downto 0);
  signal alu_tag1 : tomasulo_tag_t;
  signal cdb_use : std_logic;
begin
  cdb_use <= cdb_writable when last_unit else cdb_writable and alu_available1;
  alu_out_available <= alu_available1 when cdb_use = '1' else 'Z';
  alu_out_value <= alu_value1 when cdb_use = '1' else (others => 'Z');
  alu_out_tag <= alu_tag1 when cdb_use = '1' else (others => 'Z');
  cdb_writable_next <= cdb_writable and not alu_available1;
  alu_unit_available <=
    cdb_writable or not alu_available1 or not alu_in_available;

  sequential: process(clk, rst)
  begin
    if rising_edge(clk) then
      if refetch = '1' then
        alu_available1 <= '0';
        alu_value1 <= (others => '-');
        alu_tag1 <= (others => '-');
      elsif alu_available1 = '1' and cdb_writable /= '1' then
        -- stall
      elsif TO_01(alu_opcode, 'X')(0) = 'X' then
        alu_available1 <= alu_in_available;
        alu_value1 <= (others => 'X');
        alu_tag1 <= alu_in_tag;
      else
        alu_available1 <= alu_in_available;
        alu_tag1 <= alu_in_tag;
        case alu_opcode_t(to_integer(alu_opcode)) is
        when ALU_OP_ADD | ALU_OP_ADDU =>
          alu_value1 <= alu_in0 + alu_in1;
        when ALU_OP_SUB | ALU_OP_SUBU =>
          alu_value1 <= alu_in0 - alu_in1;
        when ALU_OP_AND =>
          alu_value1 <= alu_in0 and alu_in1;
        when ALU_OP_OR =>
          alu_value1 <= alu_in0 or alu_in1;
        when ALU_OP_XOR =>
          alu_value1 <= alu_in0 xor alu_in1;
        when ALU_OP_NOR =>
          alu_value1 <= not (alu_in0 or alu_in1);
        when ALU_OP_SLT =>
          if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
            alu_value1 <= (others => 'X');
          elsif signed(alu_in0) < signed(alu_in1) then
            alu_value1 <= to_unsigned(1, 32);
          else
            alu_value1 <= to_unsigned(0, 32);
          end if;
        when ALU_OP_SLTU =>
          if TO_01(alu_in0, 'X')(0) = 'X' or TO_01(alu_in1, 'X')(0) = 'X' then
            alu_value1 <= (others => 'X');
          elsif alu_in0 < alu_in1 then
            alu_value1 <= to_unsigned(1, 32);
          else
            alu_value1 <= to_unsigned(0, 32);
          end if;
        when ALU_OP_SLL =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_value1 <= (others => 'X');
          else
            alu_value1 <= shift_left(alu_in1, to_integer(alu_in0(4 downto 0)));
          end if;
        when ALU_OP_SRL =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_value1 <= (others => 'X');
          else
            alu_value1 <= shift_right(alu_in1, to_integer(alu_in0(4 downto 0)));
          end if;
        when ALU_OP_SRA =>
          if TO_01(alu_in0(4 downto 0), 'X')(0) = 'X' then
            alu_value1 <= (others => 'X');
          else
            alu_value1 <= unsigned(
              shift_right(signed(alu_in1), to_integer(alu_in0(4 downto 0))));
          end if;
        when others =>
          alu_value1 <= (others => '-');
        end case;
      end if;
    end if;
  end process sequential;
end architecture behavioral;

