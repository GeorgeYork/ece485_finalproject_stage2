
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

    type state_type is (FETCH, DECODE, EXECUTE, MEMORY, WRITEBACK);
    signal state : state_type := FETCH;
    
    -- Signals for pipeline stages
    signal pc, pc_byte_not_word         : STD_LOGIC_VECTOR(31 downto 0);
    signal instr      : STD_LOGIC_VECTOR(31 downto 0);
    signal alu_result : STD_LOGIC_VECTOR(31 downto 0);
    signal mem_data, data_memory_byte_not_word   : STD_LOGIC_VECTOR(31 downto 0);
    signal reg_write, reg_write_chip  : STD_LOGIC;
    signal alu_op     : STD_LOGIC_VECTOR(3 downto 0);
    signal imm        : STD_LOGIC_VECTOR(31 downto 0);

    -- Pipeline registers
    signal if_id_instr : STD_LOGIC_VECTOR(31 downto 0);
    signal if_id_pc, next_pc    : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
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

    -- control signals
    signal mem_read   : STD_LOGIC;
    signal mem_write, mem_write_chip  : STD_LOGIC;
    signal alu_src    : STD_LOGIC;
    signal branch     : STD_LOGIC;
    signal jump       : STD_LOGIC;
    signal load_addr  : STD_LOGIC;
    
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

begin

    -- PC logic
    pc_inst: pc_live
        port map (
            clk    => clk,
            reset  => reset,
            pc_in  => if_id_pc,
            pc_out => pc
        );

    -- state machine to walk through 5 cyles per instruction (not pipelined)
    -- Remove this when adding the pipeline registers and pipelining
    process(clk, reset)
        begin
            if reset = '1' then
                --state <= FETCH;
                --mem_write_chip <= '0';
                --reg_write_chip <= '0';
                --next_pc <= (others => '0');
                --tmp_next_pc <= (others => '0');
                --if_id_pc <= (others => '0');
--                write_data <= (others => '0');
            elsif rising_edge(clk) then
                case state is
                    when FETCH =>
                        --reg_write_chip <= '0';
                        --tmp_next_pc <= std_logic_vector(unsigned(pc) + 4);
                        state <= DECODE;
                    when DECODE =>
                        state <= EXECUTE;
                    when EXECUTE =>
                        state <= MEMORY;
                    when MEMORY =>
--                        if mem_write = '1' then
--                            mem_write_chip <= '1';
--                        end if;
                       
                       -- determine new PC for next instruction (assume already incremented by 4) 
--                       if branch = '1' and id_ex_reg1 /= id_ex_reg2 then
--                            next_pc <= std_logic_vector(signed(tmp_next_pc) + signed(id_ex_imm));
--                        elsif jump = '1' then
--                            next_pc <= std_logic_vector(signed(tmp_next_pc) + signed(id_ex_imm));
--                        else
--                            next_pc <= tmp_next_pc;
--                        end if;   
                        --change_pc <= '1';
                        state <= WRITEBACK;
                    when WRITEBACK =>
                            -- Write back to register file
--                        if reg_write = '1' then
--                            -- Write data to register file
--                            if load_addr = '1' then
--                               wb_data <= x"10000000";  -- Load base address of data memory
--                            else
--                               if mem_read = '1' then
--                                   wb_data <= mem_wb_data;
--                               else 
--                                   -- Normal ALU operations
--                                   wb_data <= mem_wb_alu;
--                               end if; 
--                            end if; 
                            --reg_write_chip <= '1';                       
--                        end if;
                    
                        --if_id_pc    <= next_pc;
                        state <= FETCH;
                        --mem_write_chip <= '0';
                end case;
            end if;
        end process;   
    
    -- Moore Machine, outputs determined by State
    -- FETCH
    --tmp_next_pc <= std_logic_vector(unsigned(pc) + 4) when state = FETCH else tmp_next_pc;
    -- MEMORY
    mem_write_chip <= '1' when (state = MEMORY and mem_write = '1') else '0';
    next_pc <= std_logic_vector(signed(pc) + signed(id_ex_imm)) when (state = MEMORY and branch = '1' and id_ex_reg1 /= id_ex_reg2) else
               std_logic_vector(signed(pc) + signed(id_ex_imm)) when (state = MEMORY and jump = '1') else
               std_logic_vector(unsigned(pc) + 4) when state = MEMORY else
               next_pc;
    -- WRITEBACK
    reg_write_chip <= '1' when (state = WRITEBACK and reg_write = '1') else '0';
    if_id_pc   <= next_pc when state = WRITEBACK else if_id_pc;
    wb_data <= x"10000000" when (state = WRITEBACK and reg_write = '1' and load_addr = '1') else
               mem_wb_data when (state = WRITEBACK and reg_write = '1' and mem_read = '1') else
               mem_wb_alu  when (state = WRITEBACK and reg_write = '1' and mem_read = '0') else
               wb_data;
    
    -- Instruction memory
    pc_byte_not_word <= "00" & pc(31 downto 2);  -- divide by 4 by shifting left 2, since byte addressable, not word addressable
    instr_mem_inst: instr_mem
        port map (
            --addr  => pc,
            addr  => pc_byte_not_word,
            instr => instr
        );

    -- IF/ID pipeline register
    if_id_instr <= instr;
    --if_id_pc    <= pc;

    -- Decode instruction fields
    rs1 <= if_id_instr(19 downto 15);
    rs2 <= if_id_instr(24 downto 20);
    rd  <= if_id_instr(11 downto 7);
    opcode <= if_id_instr(6 downto 0);

    -- Register file
    reg_file_inst: reg_file
        port map (
            clk       => clk,
            reg_write => reg_write_chip,
            rs1       => rs1,
            rs2       => rs2,
            rd        => wb_rd,
            data_in   => wb_data,
            data_out1 => reg1_data,
            data_out2 => reg2_data
        );

    -- Control unit
    control_unit_inst: control_unit
        port map (
            opcode    => opcode,
            reg_write => reg_write,
            mem_read  => mem_read,
            mem_write => mem_write,
            alu_src   => alu_src,
            branch    => branch,
            load_addr => load_addr,
            jump      => jump
        );

    -- Immediate generator
    immediate_generator_inst: immediate_generator
        port map (
            instr => if_id_instr,
            imm   => imm
        );

    -- ID/EX pipeline register
    id_ex_reg1 <= reg1_data;
    id_ex_reg2 <= reg2_data;
    id_ex_imm  <= imm;

    -- ALU control unit
    alu_control_inst: alu_control
        port map (
            funct3 => if_id_instr(14 downto 12),
            funct7 => if_id_instr(31 downto 25),
            alu_op => alu_op
        );

    -- mux to select alu input B
    alu_input_b <= id_ex_imm when alu_src = '1' else
                   id_ex_reg2;
    -- ALU
    alu_inst: alu
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
    data_memory_byte_not_word <= "00" & ex_mem_alu(31 downto 2);  -- divide by 4 by shifting left 2, since byte addressable, not word addressable
    data_mem_inst: data_mem
        port map (
            addr      => data_memory_byte_not_word,
            data_in   => ex_mem_reg2,
            data_out  => mem_data,
            mem_read  => mem_read,
            mem_write => mem_write
        );

    -- MEM/WB pipeline register
    mem_wb_alu  <= ex_mem_alu;
    mem_wb_data <= mem_data;

    -- Write back to register file
--    wb_data <= mem_wb_data when mem_read = '1' 
--               else x"10000000" when load_addr = '1'  -- hack for custom load_addr instruction
--               else mem_wb_alu;
               
    --wb_rd   <= id_ex_instr(11 downto 7); -- Destination register
    wb_rd   <= if_id_instr(11 downto 7); -- Destination register
end Behavioral;
