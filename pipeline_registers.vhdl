
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity pipeline_registers is
    Port (
        clk         : in  STD_LOGIC;
        reset       : in  STD_LOGIC;
        -- IF/ID pipeline registers
        if_id_reg_write : in STD_LOGIC;
        if_id_alu_src : in STD_LOGIC;
        if_id_mem_read : in STD_LOGIC;
        if_id_mem_write : in STD_LOGIC;
        if_id_branch : in STD_LOGIC;
        if_id_jump : in STD_LOGIC;
        if_id_load_addr : in STD_LOGIC;
        if_id_instr : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_pc    : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_rd    : in STD_LOGIC_VECTOR(4 downto 0);
        if_id_reg1_data  : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_reg2_data  : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_imm        : in  STD_LOGIC_VECTOR(31 downto 0);
        if_id_alu_op : in STD_LOGIC_VECTOR(3 downto 0);
        
        -- ID/EX pipeline registers
        id_ex_reg_write : inout STD_LOGIC;
        id_ex_alu_src : inout STD_LOGIC;
        id_ex_mem_read : inout STD_LOGIC;
        id_ex_mem_write : inout STD_LOGIC;
        id_ex_branch : inout STD_LOGIC;
        id_ex_jump : inout STD_LOGIC;
        id_ex_load_addr : inout STD_LOGIC;
        id_ex_rd    : inout STD_LOGIC_VECTOR(4 downto 0);
        id_ex_instr : out STD_LOGIC_VECTOR(31 downto 0);
        id_ex_pc    : inout STD_LOGIC_VECTOR(31 downto 0);
        id_ex_reg1_data  : inout  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_reg2_data  : inout  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_imm   : inout  STD_LOGIC_VECTOR(31 downto 0);
        id_ex_alu_result : in STD_LOGIC_VECTOR(31 downto 0);
        id_ex_alu_op : out STD_LOGIC_VECTOR(3 downto 0);
        -- EX/MEM pipeline registers        
        ex_mem_reg_write : inout STD_LOGIC;
        ex_mem_alu_src : inout STD_LOGIC;
        ex_mem_mem_read : inout STD_LOGIC;
        ex_mem_mem_write : inout STD_LOGIC;
        ex_mem_branch : out STD_LOGIC;
        ex_mem_jump : out STD_LOGIC;
        ex_mem_load_addr : inout STD_LOGIC;
        ex_mem_pc    : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_rd   : inout STD_LOGIC_VECTOR(4 downto 0);
        ex_mem_reg1_data : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_reg2_data : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_imm  : out STD_LOGIC_VECTOR(31 downto 0);
        ex_mem_alu_result  : inout  STD_LOGIC_VECTOR(31 downto 0);
        -- MEM/WB pipeline registers
        mem_wb_reg_write : out STD_LOGIC;
        mem_wb_alu_src : out STD_LOGIC;
        mem_wb_mem_read : out STD_LOGIC;
        mem_wb_mem_write : out STD_LOGIC;
        mem_wb_load_addr : out STD_LOGIC;
        mem_wb_rd   : out STD_LOGIC_VECTOR(4 downto 0);
        mem_wb_alu_result  : out STD_LOGIC_VECTOR(31 downto 0)
        
    );
end pipeline_registers;

architecture Behavioral of pipeline_registers is
begin
    process(clk, reset)
    begin
        if reset = '1' then
            id_ex_reg_write <= '0';
            id_ex_alu_src <= '0';
            id_ex_mem_read <= '0';
            id_ex_mem_write <= '0';
            id_ex_branch <= '0';
            id_ex_jump <= '0';
            id_ex_load_addr <= '0';
            id_ex_instr <= (others => '0');
            id_ex_pc    <= (others => '0');
            id_ex_rd   <= (others => '0');
            id_ex_reg1_data <= (others => '0');
            id_ex_reg2_data <= (others => '0');
            id_ex_imm  <= (others => '0');
            id_ex_alu_op <= (others => '0');
                        
            ex_mem_reg_write <= '0';
            ex_mem_alu_src <= '0';
            ex_mem_mem_read <= '0';
            ex_mem_mem_write <= '0';
            ex_mem_branch <= '0';
            ex_mem_jump <= '0';
            ex_mem_load_addr <= '0';
            ex_mem_pc <= (others => '0');
            ex_mem_rd   <= (others => '0');
            ex_mem_reg1_data <= (others => '0');
            ex_mem_reg2_data <= (others => '0');
            ex_mem_imm  <= (others => '0');
            ex_mem_alu_result <= (others => '0');
            
            mem_wb_reg_write <= '0';
            mem_wb_alu_src <= '0';
            mem_wb_mem_read <= '0';
            mem_wb_mem_write <= '0';
            mem_wb_load_addr <= '0'; 
            mem_wb_rd   <= (others => '0');
            mem_wb_alu_result  <= (others => '0');
            
        elsif rising_edge(clk) then
            id_ex_reg_write <= if_id_reg_write;
            id_ex_alu_src <= if_id_alu_src;
            id_ex_mem_read <= if_id_mem_read;
            id_ex_mem_write <= if_id_mem_write;
            id_ex_branch <= if_id_branch;
            id_ex_jump <= if_id_jump;
            id_ex_load_addr <= if_id_load_addr; 
            id_ex_rd    <= if_id_rd;      
            id_ex_instr <= if_id_instr;
            id_ex_pc    <= if_id_pc;
            id_ex_reg1_data  <= if_id_reg1_data;
            id_ex_reg2_data  <= if_id_reg2_data;
            id_ex_imm  <= if_id_imm;
            id_ex_alu_op <= if_id_alu_op;
                        
            ex_mem_reg_write <= id_ex_reg_write;
            ex_mem_alu_src <= id_ex_alu_src;
            ex_mem_mem_read <= id_ex_mem_read;
            ex_mem_mem_write <= id_ex_mem_write;
            ex_mem_branch <= id_ex_branch;
            ex_mem_jump <= id_ex_jump;
            ex_mem_load_addr <= id_ex_load_addr; 
            ex_mem_pc <= id_ex_pc;
            ex_mem_rd   <= id_ex_rd;      
            ex_mem_reg1_data <= id_ex_reg1_data;
            ex_mem_reg2_data <= id_ex_reg2_data;
            ex_mem_imm  <= id_ex_imm;
            ex_mem_alu_result <= id_ex_alu_result;
 
            
            mem_wb_reg_write <= ex_mem_reg_write;
            mem_wb_alu_src <= ex_mem_alu_src;
            mem_wb_mem_read <= ex_mem_mem_read;
            mem_wb_mem_write <= ex_mem_mem_write;
            mem_wb_load_addr <= ex_mem_load_addr;
            mem_wb_rd   <= ex_mem_rd;        
            mem_wb_alu_result  <= ex_mem_alu_result;
            
            --wb_data     <= mem_wb_data;
        end if;
    end process;
end Behavioral;
