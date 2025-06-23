
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
    signal pc, pc_byte_not_word         : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_pc, id_ex_pc, ex_mem_pc, next_pc    : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
    signal instr      : STD_LOGIC_VECTOR(31 downto 0);
    signal id_ex_alu_result, ex_mem_alu_result, mem_wb_alu_result : STD_LOGIC_VECTOR(31 downto 0);
    signal mem_wb_mem_data, data_memory_byte_not_word   : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_alu_op, id_ex_alu_op     : STD_LOGIC_VECTOR(3 downto 0);
    signal if_id_imm, id_ex_imm, ex_mem_imm        : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_instr, id_ex_instr : STD_LOGIC_VECTOR(31 downto 0); 
    signal if_id_reg1_data, id_ex_reg1_data, ex_mem_reg1_data  : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_reg2_data, id_ex_reg2_data, ex_mem_reg2_data  : STD_LOGIC_VECTOR(31 downto 0);
    signal ex_mem_reg1, ex_mem_reg2 : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_rs1, if_id_rs2 : STD_LOGIC_VECTOR(4 downto 0);
    signal if_id_rd, id_ex_rd, ex_mem_rd, mem_wb_rd        : STD_LOGIC_VECTOR(4 downto 0);

    -- control signals for pipeline stages
    signal if_id_reg_write, id_ex_reg_write, ex_mem_reg_write, mem_wb_reg_write  : STD_LOGIC;
    signal if_id_alu_src, id_ex_alu_src, ex_mem_alu_src, mem_wb_alu_src     : STD_LOGIC;
    signal if_id_mem_read, id_ex_mem_read, ex_mem_mem_read, mem_wb_mem_read   : STD_LOGIC;
    signal if_id_mem_write, id_ex_mem_write, ex_mem_mem_write, mem_wb_mem_write  : STD_LOGIC;
    signal if_id_branch, id_ex_branch, ex_mem_branch     : STD_LOGIC;
    signal if_id_jump, id_ex_jump, ex_mem_jump       : STD_LOGIC;
    signal if_id_load_addr, id_ex_load_addr, ex_mem_load_addr, mem_wb_load_addr  : STD_LOGIC;
    
     -- Additional signals
    signal opcode       : STD_LOGIC_VECTOR(6 downto 0);
    signal alu_input_b  : STD_LOGIC_VECTOR(31 downto 0);
    signal wb_data      : STD_LOGIC_VECTOR(31 downto 0);
 
    
    component pc_live 
    Port (
        clk     : in  STD_LOGIC;
        reset   : in  STD_LOGIC;
        pc_in   : in  STD_LOGIC_VECTOR(31 downto 0);
        pc_out  : out STD_LOGIC_VECTOR(31 downto 0)
    );
    end component;
            
    component instr_mem
        Port (
            addr : in  STD_LOGIC_VECTOR(31 downto 0);
            instr   : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

    component reg_file
        Port (
            clk     : in  STD_LOGIC;
            rs1     : in  STD_LOGIC_VECTOR(4 downto 0);
            rs2     : in  STD_LOGIC_VECTOR(4 downto 0);
            rd      : in  STD_LOGIC_VECTOR(4 downto 0);
            data_in : in  STD_LOGIC_VECTOR(31 downto 0);
            reg_write : in  STD_LOGIC;
            data_out1     : out STD_LOGIC_VECTOR(31 downto 0);
            data_out2     : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;


    component control_unit 
        Port (
        opcode      : in  STD_LOGIC_VECTOR(6 downto 0);
        reg_write   : out STD_LOGIC;
        mem_read    : out STD_LOGIC;
        mem_write   : out STD_LOGIC;
        alu_src     : out STD_LOGIC;
        branch      : out STD_LOGIC;
        load_addr   : out STD_LOGIC;  -- Custom signal for load_addr instruction
        jump        : out STD_LOGIC
        );
    end component;

    component immediate_generator is
    Port (
        instr : in  STD_LOGIC_VECTOR(31 downto 0);
        imm   : out STD_LOGIC_VECTOR(31 downto 0)
    );
    end component;

   component alu_control is
    Port (
        funct7 : in  STD_LOGIC_VECTOR(6 downto 0);
        funct3 : in  STD_LOGIC_VECTOR(2 downto 0);
        alu_op : out STD_LOGIC_VECTOR(3 downto 0)
    );
    end component;

    component alu
        Port (
            a       : in  STD_LOGIC_VECTOR(31 downto 0);
            b       : in  STD_LOGIC_VECTOR(31 downto 0);
            op      : in  STD_LOGIC_VECTOR(3 downto 0);
            result  : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

    component data_mem
        Port (
            addr    : in  STD_LOGIC_VECTOR(31 downto 0);
            data_in : in  STD_LOGIC_VECTOR(31 downto 0);
            mem_read  : in  STD_LOGIC;
            mem_write : in  STD_LOGIC;
            data_out  : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;
    
    component pipeline_registers 
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
            if_id_rd    : inout STD_LOGIC_VECTOR(4 downto 0);
            if_id_instr : in  STD_LOGIC_VECTOR(31 downto 0);
            if_id_pc    : in  STD_LOGIC_VECTOR(31 downto 0);
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
            ex_mem_alu_result  : inout STD_LOGIC_VECTOR(31 downto 0);
            -- MEM/WB pipeline registers
            mem_wb_reg_write : out STD_LOGIC;
            mem_wb_alu_src : out STD_LOGIC;
            mem_wb_mem_read : out STD_LOGIC;
            mem_wb_mem_write : out STD_LOGIC;
            mem_wb_load_addr : out STD_LOGIC;
            mem_wb_rd   : out STD_LOGIC_VECTOR(4 downto 0);
            mem_wb_alu_result  : out STD_LOGIC_VECTOR(31 downto 0)
        );
    end component;

begin

    -- PC logic
    pc_inst: pc_live
        port map (
            clk    => clk,
            reset  => reset,
            pc_in  => next_pc,
            pc_out => pc
        );
    
    -- update temporary registers to support pipelining (state machine no longer needed... as each instruction is at a different state)
    pipe_reg: pipeline_registers
        port map (
            clk    => clk,
            reset  => reset,
            -- IF/ID pipeline registers
            if_id_reg_write => if_id_reg_write,
            if_id_alu_src => if_id_alu_src,
            if_id_mem_read => if_id_mem_read,
            if_id_mem_write => if_id_mem_write,
            if_id_branch => if_id_branch,
            if_id_jump => if_id_jump,
            if_id_load_addr => if_id_load_addr,
            if_id_instr => if_id_instr,
            if_id_pc    => if_id_pc,
            if_id_rd    => if_id_rd,
            if_id_reg1_data  => if_id_reg1_data,
            if_id_reg2_data  => if_id_reg2_data,
            if_id_imm => if_id_imm,
            if_id_alu_op => if_id_alu_op,

            -- ID/EX pipeline registers
            id_ex_reg_write => id_ex_reg_write,
            id_ex_alu_src => id_ex_alu_src,
            id_ex_mem_read => id_ex_mem_read,
            id_ex_mem_write => id_ex_mem_write,
            id_ex_branch => id_ex_branch,
            id_ex_jump => id_ex_jump,
            id_ex_load_addr => id_ex_load_addr,
            id_ex_rd    => id_ex_rd,
            id_ex_instr => id_ex_instr,
            id_ex_pc    => id_ex_pc,
            id_ex_reg1_data  => id_ex_reg1_data,
            id_ex_reg2_data  => id_ex_reg2_data,
            id_ex_imm   => id_ex_imm,
            id_ex_alu_result => id_ex_alu_result,
            id_ex_alu_op => id_ex_alu_op,
            -- EX/MEM pipeline registers
            ex_mem_reg_write => ex_mem_reg_write,
            ex_mem_alu_src => ex_mem_alu_src,
            ex_mem_mem_read => ex_mem_mem_read,
            ex_mem_mem_write => ex_mem_mem_write,
            ex_mem_branch => ex_mem_branch,
            ex_mem_jump => ex_mem_jump,
            ex_mem_load_addr => ex_mem_load_addr,
            ex_mem_pc => ex_mem_pc,
            ex_mem_rd   => ex_mem_rd,
            ex_mem_reg1_data => ex_mem_reg1_data,
            ex_mem_reg2_data => ex_mem_reg2_data,
            ex_mem_imm  => ex_mem_imm,
            ex_mem_alu_result  => ex_mem_alu_result,
            -- MEM/WB pipeline registers
            mem_wb_reg_write => mem_wb_reg_write,
            mem_wb_alu_src => mem_wb_alu_src,
            mem_wb_mem_read => mem_wb_mem_read,
            mem_wb_mem_write => mem_wb_mem_write,
            mem_wb_load_addr => mem_wb_load_addr,
            mem_wb_rd   => mem_wb_rd,
            mem_wb_alu_result  => mem_wb_alu_result
        );
    
    -- IF units 
    -- Instruction memory
    pc_byte_not_word <= "00" & pc(31 downto 2);  -- divide by 4 by shifting left 2, since byte addressable, not word addressable
    instr_mem_inst: instr_mem
        port map (
            addr  => pc_byte_not_word,
            instr => instr
        );   
    -- IF/ID pipeline registers
    if_id_instr <= instr;
    if_id_pc    <= pc;

    -- Decode instruction fields
    if_id_rs1 <= if_id_instr(19 downto 15);
    if_id_rs2 <= if_id_instr(24 downto 20);
    if_id_rd  <= if_id_instr(11 downto 7);
    opcode <= if_id_instr(6 downto 0);

    -- Control unit
    control_unit_inst: control_unit
        port map (
            opcode    => opcode,
            reg_write => if_id_reg_write,
            mem_read  => if_id_mem_read,
            mem_write => if_id_mem_write,
            alu_src   => if_id_alu_src,
            branch    => if_id_branch,
            load_addr => if_id_load_addr,
            jump      => if_id_jump
        );
--------------------------------------------------------------------------------
    -- ID units
    -- Register file [used in ID and WB stages]
    reg_file_inst: reg_file
        port map (
            clk       => clk,
            reg_write => mem_wb_reg_write,
            rs1       => if_id_rs1,
            rs2       => if_id_rs2,
            rd        => mem_wb_rd,
            data_in   => wb_data,
            data_out1 => if_id_reg1_data,
            data_out2 => if_id_reg2_data
        );    
    -- Immediate generator
        immediate_generator_inst: immediate_generator
            port map (
                instr => if_id_instr,
                imm   => if_id_imm
            );
            
    -- ALU control unit
        alu_control_inst: alu_control
                port map (
                    funct3 => if_id_instr(14 downto 12),
                    funct7 => if_id_instr(31 downto 25),
                    alu_op => if_id_alu_op
                );
                
    -- ID/EX pipeline registers

-----------------------------------------------------------
    -- EX units
    -- mux to select alu input B
    alu_input_b <= id_ex_imm when id_ex_alu_src = '1' else
                   id_ex_reg2_data;
    -- ALU
    alu_inst: alu
        port map (
            a      => id_ex_reg1_data,
            b      => alu_input_b,
            op     => id_ex_alu_op,
            result => id_ex_alu_result
        );

    -- EX/MEM pipeline register

----------------------------------------------------------------------------------------
    --  MEM units
    
    -- Data memory
    data_memory_byte_not_word <= "00" & ex_mem_alu_result(31 downto 2);  -- divide by 4 by shifting left 2, since byte addressable, not word addressable
    data_mem_inst: data_mem
        port map (
            addr      => data_memory_byte_not_word,
            data_in   => ex_mem_reg2_data,
            data_out  => mem_wb_mem_data,
            mem_read  => ex_mem_mem_read,
            mem_write => ex_mem_mem_write
        );

    -- MEM/WB pipeline register

    
    next_pc <= std_logic_vector(signed(ex_mem_pc) + signed(ex_mem_imm)) when (ex_mem_branch = '1' and ex_mem_reg1_data /= ex_mem_reg2_data) else
               std_logic_vector(signed(ex_mem_pc) + signed(ex_mem_imm)) when (ex_mem_jump = '1') else
               std_logic_vector(unsigned(if_id_pc) + 4); -- note: this happens during IF !!! 1st two during MEM
------------------------------------------------------------------------------------------
    -- WB Units
    
    -- MUX to write back to register file
    wb_data <= mem_wb_mem_data when mem_wb_mem_read = '1' 
               else x"10000000" when mem_wb_load_addr = '1'  -- hack for custom load_addr instruction
               else mem_wb_alu_result;      
   
end Behavioral;
