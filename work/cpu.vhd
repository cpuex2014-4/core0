library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.serial.all;
use work.kakeudon.all;

entity cpu is
  generic (
    debug_out : boolean;
    debug_out_commit : boolean;
    bypass_io : boolean;
    rs_baudrate : real;
    rs_stopbit : real);
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

end cpu;

architecture behavioral of cpu is
  signal mem_enable : std_logic;
  signal mem_isstore : std_logic;
  signal mem_addr : unsigned(29 downto 0);
  signal mem_bytes : unsigned(3 downto 0);
  signal mem_tag : tomasulo_tag_t;
  signal mem_data_write : unsigned(31 downto 0);
  signal mem_avail_read : std_logic;
  signal mem_data_read : unsigned(31 downto 0);
  signal mem_tag_read : tomasulo_tag_t;
  signal mem_refetch : std_logic;
  signal mem_inst_addr : unsigned(29 downto 0);
  signal mem_inst_data : unsigned(31 downto 0);

  signal rs232c_recv_empty : std_logic;
  signal rs232c_recv_top : unsigned(7 downto 0);
  signal rs232c_recv_consume : std_logic;
  signal rs232c_send_full : std_logic;
  signal rs232c_send_bottom : unsigned(7 downto 0);
  signal rs232c_send_push : std_logic;
begin
  core_unit : core
  generic map (
    debug_out => debug_out,
    debug_out_commit => debug_out_commit)
  port map (
    mem_enable => mem_enable,
    mem_isstore => mem_isstore,
    mem_addr => mem_addr,
    mem_bytes => mem_bytes,
    mem_tag => mem_tag,
    mem_data_write => mem_data_write,
    mem_avail_read => mem_avail_read,
    mem_data_read => mem_data_read,
    mem_tag_read => mem_tag_read,
    mem_refetch => mem_refetch,
    mem_inst_addr => mem_inst_addr,
    mem_inst_data => mem_inst_data,
    clk => clk,
    rst => rst);

  mem : memory_controller
  generic map (
    debug_out => debug_out,
    debug_out_commit => debug_out_commit)
  port map (
    clk => clk,
    enable => mem_enable,
    isstore => mem_isstore,
    addr => mem_addr,
    bytes => mem_bytes,
    tag => mem_tag,
    data_write => mem_data_write,
    avail_read => mem_avail_read,
    data_read => mem_data_read,
    tag_read => mem_tag_read,
    refetch => mem_refetch,
    inst_addr => mem_inst_addr,
    inst_data => mem_inst_data,
    ZD => ZD,
    ZDP => ZDP,
    ZA => ZA,
    XE1 => XE1,
    E2A => E2A,
    XE3 => XE3,
    XZBE => XZBE,
    XGA => XGA,
    XWA => XWA,
    XZCKE => XZCKE,
    ZCLKMA => ZCLKMA,
    ADVA => ADVA,
    XFT => XFT,
    XLBO => XLBO,
    ZZA => ZZA,
    rs232c_recv_empty => rs232c_recv_empty,
    rs232c_recv_top => rs232c_recv_top,
    rs232c_recv_consume => rs232c_recv_consume,
    rs232c_send_full => rs232c_send_full,
    rs232c_send_bottom => rs232c_send_bottom,
    rs232c_send_push => rs232c_send_push);

  non_bypass_io : if not bypass_io generate
    rs232c_unit : io_rs232c
    generic map (
      debug_out => debug_out,
      baudrate => rs_baudrate,
      stopbit => rs_stopbit)
    port map (
      clk => clk,
      rst => rst,
      RS_RX => RS_RX,
      RS_TX => RS_TX,
      recv_empty => rs232c_recv_empty,
      recv_top => rs232c_recv_top,
      recv_consume => rs232c_recv_consume,
      send_full => rs232c_send_full,
      send_bottom => rs232c_send_bottom,
      send_push => rs232c_send_push);
  end generate non_bypass_io;

  bypassing_io : if bypass_io generate
    recv_into_fifo : process(clk, rst)
      type t_char_file is file of character;
      file read_file : t_char_file open read_mode is "in.dat";
      variable init : boolean := false;
      variable rdend : boolean := false;
      variable ch : character;
    begin
      if not init then
        init := true;
        if endfile(read_file) then
          rdend := true;
        else
          read(read_file, ch);
        end if;
      end if;
      if rst = '1' then
      elsif rising_edge(clk) then
        if not rdend and rs232c_recv_consume = '1' then
          if endfile(read_file) then
            rdend := true;
          else
            read(read_file, ch);
          end if;
        end if;
      end if;
      if rdend then
        rs232c_recv_empty <= '1';
        rs232c_recv_top <= (others => 'X');
      else
        rs232c_recv_empty <= '0';
        rs232c_recv_top <= to_unsigned(character'pos(ch), 8);
      end if;
    end process recv_into_fifo;

    send_from_fifo : process(clk, rst)
      type t_char_file is file of character;
      file write_file : t_char_file open write_mode is "out.dat";
    begin
      if rst = '1' then
      elsif rising_edge(clk) then
        if rs232c_send_push = '1' then
          write(write_file, character'val(to_integer(rs232c_send_bottom)));
        end if;
      end if;
      rs232c_send_full <= '0';
    end process send_from_fifo;
  end generate bypassing_io;
end behavioral;

