
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity pipeline_registers is
    Port (
        clk         : in  STD_LOGIC;
        reset       : in  STD_LOGIC;
        -- IF/ID pipeline registers
        if_id_instr : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_pc    : in  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_instr : out STD_LOGIC_VECTOR(31 downto 0);
        id_ex_pc    : out STD_LOGIC_VECTOR(31 downto 0);
        -- ID/EX pipeline registers
        id_ex_reg1  : in  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_reg2  : in  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_imm   : in  STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_reg1 : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_reg2 : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_imm  : out STD_LOGIC_VECTOR(31 downto 0);
        -- EX/MEM pipeline registers
        ex_mem_alu  : in  STD_LOGIC_VECTOR(31 downto 0);
        mem_wb_alu  : out STD_LOGIC_VECTOR(31 downto 0);
        -- MEM/WB pipeline registers
        mem_wb_data : in  STD_LOGIC_VECTOR(31 downto 0);
        wb_data     : out STD_LOGIC_VECTOR(31 downto 0)
    );
end pipeline_registers;

architecture Behavioral of pipeline_registers is
begin
    process(clk, reset)
    begin
        if reset = '1' then
            id_ex_instr <= (others => '0');
            id_ex_pc    <= (others => '0');
            ex_mem_reg1 <= (others => '0');
            ex_mem_reg2 <= (others => '0');
            ex_mem_imm  <= (others => '0');
            mem_wb_alu  <= (others => '0');
            wb_data     <= (others => '0');
        elsif rising_edge(clk) then
            id_ex_instr <= if_id_instr;
            id_ex_pc    <= if_id_pc;
            ex_mem_reg1 <= id_ex_reg1;
            ex_mem_reg2 <= id_ex_reg2;
            ex_mem_imm  <= id_ex_imm;
            mem_wb_alu  <= ex_mem_alu;
            wb_data     <= mem_wb_data;
        end if;
    end process;
end Behavioral;
