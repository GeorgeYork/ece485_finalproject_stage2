library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity instr_mem is
    Port (
        addr    : in  STD_LOGIC_VECTOR(31 downto 0);
        instr   : out STD_LOGIC_VECTOR(31 downto 0)
    );
end instr_mem;

architecture Behavioral of instr_mem is
    type memory_array is array (0 to 255) of STD_LOGIC_VECTOR(31 downto 0);
    signal memory : memory_array := (
        0 => x"00900293", -- addi x5, x0, 9         000000001001 00000 000 00101 0010011
        1 => x"10000317", -- load_addr x6, array (custom instruction), where array is 0x10000000
        2 => x"00032383", -- lw x7, 0(x6)           000000000000 00110 010 00111 0000011
        3 => x"00430313", -- loop: addi x6, x6, 4   000000000100 00110 000 00110 0010011
        4 => x"00032503", --       lw x10, 0(x6)    000000000000 00110 010 01010 0000011
        5 => x"007503B3", --       add x7, x10, x7 0000000 00111 01010 000 00111 0110011
        6 => x"FFF28293", --       subi x5, x5, 1 (really addi x5, x5, -1)  111111111111 00101 000 00101 0010011
        7 => x"FC0290E3", --       bne x5, x0, loop   1 111110 00000 00101 001 0000 1 1100011 [note: assumes PC is NOT already incremented by 4]
        8 => x"0000006f", -- done: j done          00000000000000000000 00000 1101111  [note: assumes PC is NOT already incremented by 4]
        others => (others => '0')
    );
begin
    process(addr)
    begin
        instr <= memory(to_integer(unsigned(addr(7 downto 0))));
    end process;
end Behavioral;
