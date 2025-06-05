
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity riscv_pipeline is
    Port (
        clk     : in  STD_LOGIC;
        reset   : in  STD_LOGIC
    );
end riscv_pipeline;

architecture Behavioral of riscv_pipeline is

    -- Signals for pipeline stages
    signal pc         : STD_LOGIC_VECTOR(31 downto 0);
    signal instr      : STD_LOGIC_VECTOR(31 downto 0);
    signal alu_result : STD_LOGIC_VECTOR(31 downto 0);
    signal mem_data   : STD_LOGIC_VECTOR(31 downto 0);
    signal reg_write  : STD_LOGIC;
    signal alu_op     : STD_LOGIC_VECTOR(3 downto 0);
    signal imm        : STD_LOGIC_VECTOR(31 downto 0);

    -- Pipeline registers
    signal if_id_instr : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_pc    : STD_LOGIC_VECTOR(31 downto 0);
    signal id_ex_reg1  : STD_LOGIC_VECTOR(31 downto 0);
    signal id_ex_reg2  : STD_LOGIC_VECTOR(31 downto 0);
    signal id_ex_imm   : STD_LOGIC_VECTOR(31 downto 0);
    signal ex_mem_alu  : STD_LOGIC_VECTOR(31 downto 0);
    signal ex_mem_reg2 : STD_LOGIC_VECTOR(31 downto 0);
    signal mem_wb_alu  : STD_LOGIC_VECTOR(31 downto 0);
    signal mem_wb_data : STD_LOGIC_VECTOR(31 downto 0);

    -- Additional signals
    signal rs1, rs2, rd : STD_LOGIC_VECTOR(4 downto 0);
    signal opcode       : STD_LOGIC_VECTOR(6 downto 0);
    signal reg1_data, reg2_data : STD_LOGIC_VECTOR(31 downto 0);
    signal alu_input_b  : STD_LOGIC_VECTOR(31 downto 0);
    signal wb_data      : STD_LOGIC_VECTOR(31 downto 0);
    signal wb_rd        : STD_LOGIC_VECTOR(4 downto 0);

begin

    -- PC logic
    pc_inst: entity work.pc
        port map (
            clk    => clk,
            reset  => reset,
            pc_in  => if_id_pc,
            pc_out => pc
        );

    -- Instruction memory
    instr_mem_inst: entity work.instr_mem
        port map (
            addr  => pc,
            instr => instr
        );

    -- IF/ID pipeline register
    if_id_instr <= instr;
    if_id_pc    <= pc;

    -- Decode instruction fields
    rs1 <= if_id_instr(19 downto 15);
    rs2 <= if_id_instr(24 downto 20);
    rd  <= if_id_instr(11 downto 7);
    opcode <= if_id_instr(6 downto 0);

    -- Register file
    reg_file_inst: entity work.reg_file
        port map (
            clk       => clk,
            reg_write => reg_write,
            rs1       => rs1,
            rs2       => rs2,
            rd        => wb_rd,
            data_in   => wb_data,
            data_out1 => reg1_data,
            data_out2 => reg2_data
        );

    -- Control unit
    control_unit_inst: entity work.control_unit
        port map (
            opcode    => opcode,
            reg_write => reg_write,
            mem_read  => mem_read,
            mem_write => mem_write,
            alu_src   => alu_src,
            branch    => branch,
            jump      => jump
        );

    -- Immediate generator
    immediate_generator_inst: entity work.immediate_generator
        port map (
            instr => if_id_instr,
            imm   => imm
        );

    -- ID/EX pipeline register
    id_ex_reg1 <= reg1_data;
    id_ex_reg2 <= reg2_data;
    id_ex_imm  <= imm;

    -- ALU control unit
    alu_control_inst: entity work.alu_control
        port map (
            funct3 => if_id_instr(14 downto 12),
            funct7 => if_id_instr(31 downto 25),
            alu_op => alu_op
        );

    -- ALU
    alu_inst: entity work.alu
        port map (
            a      => id_ex_reg1,
            b      => alu_input_b,
            op     => alu_op,
            result => alu_result
        );

    -- EX/MEM pipeline register
    ex_mem_alu  <= alu_result;
    ex_mem_reg2 <= id_ex_reg2;

    -- Data memory
    data_mem_inst: entity work.data_mem
        port map (
            addr      => ex_mem_alu,
            data_in   => ex_mem_reg2,
            data_out  => mem_data,
            mem_read  => mem_read,
            mem_write => mem_write
        );

    -- MEM/WB pipeline register
    mem_wb_alu  <= ex_mem_alu;
    mem_wb_data <= mem_data;

    -- Write back to register file
    wb_data <= mem_wb_data when mem_read = '1' else mem_wb_alu;
    wb_rd   <= id_ex_instr(11 downto 7); -- Destination register

end Behavioral;
