library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity memory_controller is
  generic (
    debug_out : boolean;
    debug_out_commit : boolean;
    bypass_program_loading : boolean);
  port (
    clk : in std_logic;
    -- main read/write
    enable : in std_logic;
    isstore : in std_logic;
    addr : in unsigned(29 downto 0);
    bytes : in unsigned(3 downto 0);
    tag : in tomasulo_tag_t;
    data_write : in unsigned(31 downto 0);
    avail_read : out std_logic;
    data_read : out unsigned(31 downto 0);
    tag_read : out tomasulo_tag_t;
    -- refetch
    refetch : in std_logic;
    -- instruction
    inst_addr : in unsigned(29 downto 0);
    inst_data : out unsigned(31 downto 0);
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
    -- RS-232C I/O Controller
    rs232c_recv_empty : in std_logic;
    rs232c_recv_top : in unsigned(7 downto 0);
    rs232c_recv_consume : out std_logic := '0';
    rs232c_send_full : in std_logic;
    rs232c_send_bottom : out unsigned(7 downto 0);
    rs232c_send_push : out std_logic := '0');
end entity memory_controller;

architecture behavioral of memory_controller is
  type instruction_memory_t is
    array(0 to 32767) of unsigned(31 downto 0);
  signal instruction_memory : instruction_memory_t;
  attribute ram_style of instruction_memory: signal is "block";
  signal instruction_rom : instruction_rom_t := instruction_rom_data;
  attribute ram_style of instruction_rom: signal is "block";

  signal instruction_source : unsigned(1 downto 0);
  signal inst_data_0 : unsigned(31 downto 0);
  signal inst_data_1 : unsigned(31 downto 0);

  signal non_sram_data_delay1 : unsigned(31 downto 0);
  signal non_sram_data_delay2 : unsigned(31 downto 0);
  signal read_data_from_sram_delay1 : std_logic;
  signal read_data_from_sram_delay2 : std_logic;
  signal read_enable_delay1 : std_logic := '0';
  signal read_enable_delay2 : std_logic := '0';
  signal tag_delay1 : tomasulo_tag_t;
  signal tag_delay2 : tomasulo_tag_t;
  signal isstore_delay1 : std_logic;
  signal isstore_delay2 : std_logic;
  signal data_write_delay1 : unsigned(31 downto 0);
  signal data_write_delay2 : unsigned(31 downto 0);

  signal instruction_memory_to_bypass : instruction_memory_t;
  signal instruction_memory_to_bypass_available : boolean := false;
  signal instruction_memory_to_bypass_used : boolean := false;
begin
  XE1 <= '0';
  E2A <= '1';
  XE3 <= '0';
  XGA <= '0';
  XZCKE <= '0';
  ZCLKMA <= (others => clk);
  ADVA <= '0';
  XLBO <= '1';
  ZZA <= '0';
  XFT <= '1';
  XZBE <= (others => '0');
  ZA <= std_logic_vector(addr(19 downto 0));
  ZDP <= (others => 'Z') when isstore_delay2 /= '1' else (others => '0');
  ZD <= (others => 'Z') when isstore_delay2 /= '1' else
        std_logic_vector(data_write_delay2);
  avail_read <= read_enable_delay2;
  data_read <=
    (others => 'X') when TO_X01(read_data_from_sram_delay2) = 'X' else
    unsigned(ZD) when read_data_from_sram_delay2 = '1' else
    non_sram_data_delay2;
  tag_read <= tag_delay2;
  XWA <= not (isstore and enable);
  sequential: process(instruction_memory_to_bypass_available, clk)
    variable next_rs232c_recv_consume : std_logic;
    variable next_rs232c_send_push : std_logic;
    variable next_rs232c_send_bottom : unsigned(7 downto 0);
    variable next_isstore_delay1 : std_logic;
  begin
    if bypass_program_loading then
      if instruction_memory_to_bypass_available and
         not instruction_memory_to_bypass_used then
        instruction_memory <= instruction_memory_to_bypass;
        instruction_memory_to_bypass_used <= true;
      end if;
    end if;
    if rising_edge(clk) then
      next_rs232c_recv_consume := '0';
      next_rs232c_send_push := '0';
      next_rs232c_send_bottom := (others => '-');
      next_isstore_delay1 := '0';

      if enable = '1' then
        if isstore = '0' then
          if TO_01(addr,'X')(0) = 'X' then
            read_data_from_sram_delay1 <= 'X';
            non_sram_data_delay1 <= (others => '-');
          elsif bypass_program_loading and
                addr(26 downto 15) = "000000000000" then
            read_data_from_sram_delay1 <= '0';
            non_sram_data_delay1 <=
              instruction_memory(to_integer(addr(14 downto 0)));
          elsif addr(29 downto 20) = "0000000000" then
            read_data_from_sram_delay1 <= '1';
            non_sram_data_delay1 <= (others => '-');
          elsif addr & "00" = x"FFFF0000" then
            read_data_from_sram_delay1 <= '0';
            non_sram_data_delay1 <=
              (31 downto 2 => '0',
               1 => '0',
               0 => not rs232c_recv_empty);
          elsif addr & "00" = x"FFFF0004" then
            read_data_from_sram_delay1 <= '0';
            next_rs232c_recv_consume := not rs232c_recv_empty;
            non_sram_data_delay1 <= (31 downto 8 => '0') & rs232c_recv_top;
          elsif addr & "00" = x"FFFF0008" then
            read_data_from_sram_delay1 <= '0';
            non_sram_data_delay1 <=
              (31 downto 2 => '0',
               1 => '0',
               0 => not rs232c_send_full);
          else
            read_data_from_sram_delay1 <= '-';
            non_sram_data_delay1 <= (others => '-');
          end if;
        else
          assert TO_01(addr,'X')(0) /= 'X'
            report "metavalue detected in addr"
              severity failure;
          assert TO_01(data_write,'X')(0) /= 'X'
            report "metavalue detected in data_write"
              severity failure;
          assert not debug_out_commit
            report "Memory[" & hex_of_word(addr&"00") & "] <- " &
              hex_of_word(data_write)
                severity note;
          if addr(29 downto 20) = "0000000000" then
            next_isstore_delay1 := '1';
          elsif addr & "00" = x"FFFF000C" then
            next_rs232c_send_push := not rs232c_send_full;
            next_rs232c_send_bottom := data_write(7 downto 0);
          end if;
          read_data_from_sram_delay1 <= '-';
          non_sram_data_delay1 <= (others => '-');
        end if;
      end if;

      read_enable_delay1 <= not isstore and enable and not refetch;
      read_enable_delay2 <= read_enable_delay1 and not refetch;
      tag_delay1 <= tag;
      tag_delay2 <= tag_delay1;
      isstore_delay1 <= next_isstore_delay1;
      isstore_delay2 <= isstore_delay1;
      data_write_delay1 <= data_write;
      data_write_delay2 <= data_write_delay1;

      read_data_from_sram_delay2 <= read_data_from_sram_delay1;
      non_sram_data_delay2 <= non_sram_data_delay1;

      if (isstore and enable) = '1' then
        assert TO_01(addr,'X')(0) /= 'X'
          report "metavalue detected in addr"
            severity warning;
        if addr(26 downto 15) = "0000000000" then
          instruction_memory(to_integer(addr(14 downto 0))) <=
            data_write;
        end if;
      end if;

      -- if TO_01(inst_addr, 'X')(0) = 'X' then
      --   inst_data <= (others => '-');
      -- elsif inst_addr(26 downto 15) = "000000000000" then
      --   inst_data <=
      --     instruction_memory(to_integer(inst_addr(14 downto 0)));
      -- elsif inst_addr(26 downto 6) = "111111100000000000000" then
      --   inst_data <=
      --     instruction_rom(to_integer(inst_addr(5 downto 0)));
      -- else
      --   inst_data <= (others => '0');
      -- end if;
      if TO_01(inst_addr, 'X')(0) = 'X' then
        instruction_source <= (others => '-');
        inst_data_0 <= (others => '-');
        inst_data_1 <= (others => '-');
      else
        if inst_addr(26 downto 15) = "000000000000" then
          instruction_source <= "00";
        elsif inst_addr(26 downto 6) = "111111100000000000000" then
          instruction_source <= "01";
        else
          instruction_source <= "11";
        end if;
        inst_data_0 <= instruction_memory(to_integer(inst_addr(14 downto 0)));
        inst_data_1 <= instruction_rom(to_integer(inst_addr(5 downto 0)));
      end if;

      rs232c_recv_consume <= next_rs232c_recv_consume;
      rs232c_send_push <= next_rs232c_send_push;
      rs232c_send_bottom <= next_rs232c_send_bottom;
    end if;
  end process sequential;
  inst_data <=
    (others => 'X') when TO_01(instruction_source, 'X')(0) = 'X' else
    inst_data_0 when instruction_source = "00" else
    inst_data_1 when instruction_source = "01" else
    (others => '0');

  bypassing_program_loading : if bypass_program_loading generate
    foo: process
      type t_char_file is file of character;
      file read_file : t_char_file open read_mode is "in.dat";
      variable ch0, ch1, ch2, ch3 : character;
      variable w : unsigned(31 downto 0);
      variable b : boolean;
      variable i : integer;
    begin
      b := true;
      i := 0;
      while b loop
        read(read_file, ch0);
        read(read_file, ch1);
        read(read_file, ch2);
        read(read_file, ch3);
        w := to_unsigned(character'pos(ch0), 8) &
             to_unsigned(character'pos(ch1), 8) &
             to_unsigned(character'pos(ch2), 8) &
             to_unsigned(character'pos(ch3), 8);
        if w = x"FFFFFFFF" then
          b := false;
        else
          instruction_memory_to_bypass(i) <= w;
          i := i + 1;
        end if;
      end loop;
      for j in 1 to 32 loop
        instruction_memory_to_bypass(i) <= x"00000000";
        i := i + 1;
      end loop;
      instruction_memory_to_bypass_available <= true;
      wait;
    end process foo;
  end generate bypassing_program_loading;
end architecture behavioral;

