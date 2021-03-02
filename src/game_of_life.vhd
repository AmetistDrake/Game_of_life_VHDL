LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_unsigned.ALL;
USE ieee.numeric_std.ALL;
USE std.textio.ALL;

ENTITY game_of_life IS
    PORT (
        clk : IN STD_LOGIC := '0';
        start : IN STD_LOGIC := '0'
    );
END ENTITY;

ARCHITECTURE arch OF game_of_life IS
    CONSTANT W : INTEGER := 15;
    CONSTANT H : INTEGER := 15;
    CONSTANT max_tick : INTEGER := 30;
    CONSTANT portion_size : INTEGER := W; -- max W
    CONSTANT seed1 : POSITIVE := 3;
    CONSTANT seed2 : POSITIVE := 7;
    CONSTANT filename : STRING := "output/game_of_life.txt";

    TYPE init_map_type IS ARRAY (0 TO (H - 1)) OF STD_LOGIC_VECTOR(0 TO (W - 1));
    TYPE init_state_type IS (random, glider, achim_p4, crystal, achim_p16, custom);
    CONSTANT init_state : init_state_type := random; --achim_p16;-- glider; -- achim_p4;

    CONSTANT achim_p16_init_map : init_map_type := (
        "000000001100000",
        "000000001010000",
        "000100001011000",
        "001100000100000",
        "010010000000000",
        "011100000000000",
        "000000000000000",
        "000000000001110",
        "000000000010010",
        "000000000001100",
        "000001000001000",
        "000110100000000",
        "000010100000000",
        "000001100000000",
        "000000000000000"
    );

    CONSTANT crystal_init_map: init_map_type := (
        "000000000000000",
        "000000000000000",
        "000010000000000",
        "000001000110000",
        "000111000110000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000"
    );

    CONSTANT achim_p4_init_map: init_map_type := (
        "000000010000000",
        "000000101000000",
        "000001010100000",
        "000001000100000",
        "001101010101100",
        "001010000010100",
        "000001111100000",
        "000000000000000",
        "000000010000000",
        "000000101000000",
        "000000010000000",
        "000000000000000",
        "000000000000000",
        "000000000000000",
        "000000000000000"
    );
    
    CONSTANT glider_init_map : init_map_type := ( 
        "010000000000000",
        "001000000000000",
        "111000000000000",
        OTHERS => (others => '0')
    );

    CONSTANT custom_init_map : init_map_type := ( 
        "110000000000000",
        "001000000000000",
        OTHERS => (others => '0')
    );

    SIGNAL init_map : init_map_type := (OTHERS => (OTHERS => '0'));

    -- map padded with zeros for calculations
    TYPE reg_map_type IS ARRAY (0 TO (H + 1)) OF STD_LOGIC_VECTOR(0 TO (W + 1));
    SIGNAL curr_map : reg_map_type := (OTHERS => (OTHERS => '0'));
    SIGNAL new_map : reg_map_type := (OTHERS => (OTHERS => '0'));

    SIGNAL tick : INTEGER := 0;
    SIGNAL row_count : INTEGER := 0;
    SIGNAL col_count : INTEGER := 0;

    -- 3 x Width row_blocks of the map
    -- MSB and LSB should be changed? This way its easier to iterate in loops. 
    TYPE row_block_type IS ARRAY (0 TO 2) OF STD_LOGIC_VECTOR(0 TO (W + 1));
    SIGNAL row_block : row_block_type;

    -- a portion of the row_block
    TYPE portion_type IS ARRAY (0 TO 2) OF STD_LOGIC_VECTOR(0 TO (portion_size + 1));
    SIGNAL portion : portion_type;

    TYPE block_type IS ARRAY (0 TO 2) OF STD_LOGIC_VECTOR(0 TO 2);

    -- state memory (SM)
    TYPE state_type IS (init, load_row, load_portion, eval_portion, calculated);
    SIGNAL state : state_type := init;

    -- generate random init map
    FUNCTION generate_init_map RETURN init_map_type IS
        VARIABLE s1 : POSITIVE := seed1;
        VARIABLE s2 : POSITIVE := seed2;
        VARIABLE re : REAL;
        VARIABLE new_init_map : init_map_type;
    BEGIN
        FOR i IN 0 TO (H - 1) LOOP
            FOR j IN 0 TO (W - 1) LOOP
                uniform(s1, s2, re);
                new_init_map(i)(j) := '1' WHEN re > 0.5 ELSE
                '0';
            END LOOP;
        END LOOP;
        RETURN new_init_map;
    END FUNCTION;
BEGIN
    PROCESS (clk, start)
        VARIABLE living_count : INTEGER := 0; -- need in computation step
        VARIABLE block3x3 : block_type := (OTHERS => (OTHERS => '0')); -- to easily debug the computation step  
        VARIABLE new_n, new_m : INTEGER := 0; -- indexes for the new map
        VARIABLE row : STD_LOGIC_VECTOR(0 TO W - 1); -- tmp just for output the non padded map
        FILE out_file : text; -- for file output
        VARIABLE out_line : line; -- for file output
    BEGIN
        IF rising_edge(clk) THEN
            CASE state IS
                WHEN init =>
                    -- next state logic (NSL) 
                    IF (start = '1') THEN
                        state <= load_row;
                    ELSE
                        state <= init;
                    END IF;

                    -- out function logic (OFL)

                    -- if random map enabled, generate random map
                    CASE init_state IS
                        WHEN glider => init_map <= glider_init_map;
                        WHEN achim_p4 => init_map <= achim_p4_init_map;
                        WHEN achim_p16 => init_map <= achim_p16_init_map;
                        WHEN crystal => init_map <= crystal_init_map;
                        WHEN random => init_map <= generate_init_map;
                        WHEN custom => init_map <= custom_init_map;
                        WHEN OTHERS => null;
                    END CASE;

                    IF start = '1' THEN
                        file_open(out_file, filename, write_mode);
                        -- put init map into reg_map
                        FOR i IN 1 TO H LOOP
                            FOR j IN 1 TO W LOOP
                                curr_map(i)(j) <= init_map(i - 1)(j - 1);
                            END LOOP;
                        END LOOP;
                    END IF;

                WHEN load_row =>
                    -- NSL
                    IF row_count < H THEN
                        state <= load_portion;
                    ELSE
                        state <= calculated;
                    END IF;

                    -- OFL
                    IF row_count < H THEN
                        col_count <= 0;

                        -- fill row_block
                        FOR i IN 0 TO 2 LOOP
                            row_block(i) <= curr_map(row_count + i);
                            -- report to_string(row_block(i));
                        END LOOP;
                        row_count <= row_count + 1;
                    END IF;
                WHEN load_portion =>
                    -- NSL
                    IF col_count < W THEN
                        state <= eval_portion;
                    ELSE
                        state <= load_row;
                    END IF;

                    -- OFL 
                    IF col_count < W THEN
                        -- fill portion
                        FOR i IN 0 TO 2 LOOP
                            FOR j IN col_count TO col_count + portion_size + 1 LOOP
                                IF j < W + 2 THEN
                                    portion(i)(j - col_count) <= row_block(i)(j);
                                END IF;
                            END LOOP;
                            -- REPORT to_string(portion(i));
                        END LOOP; -- fill_portion

                        col_count <= col_count + portion_size;
                    END IF;
                WHEN eval_portion =>
                    -- NSL
                    state <= load_portion;

                    -- OFL
                    -- loop through portions

                    FOR i IN 0 TO portion_size - 1 LOOP
                        IF (col_count - portion_size) + i < W THEN
                            new_n := row_count;
                            new_m := (col_count - portion_size) + i + 1;
                            living_count := 0;

                            -- report "n: " & to_string(new_n) & " m: " & to_string(new_m);

                            -- assign block
                            FOR n IN 0 TO 2 LOOP
                                FOR m IN 0 TO 2 LOOP
                                    block3x3(n)(m) := portion(n)(i + m);
                                END LOOP;
                            END LOOP;

                            -- for n in 0 to 2 loop
                            --     report to_string(block3x3(n));
                            -- end loop;

                            -- count living cells
                            FOR n IN 0 TO 2 LOOP
                                FOR m IN 0 TO 2 LOOP
                                    IF NOT (n = 1 AND m = 1) AND block3x3(n)(m) = '1' THEN
                                        living_count := living_count + 1;
                                    END IF;
                                END LOOP;
                            END LOOP;
                            -- report to_string(living_count);

                            -- assign value to new_map based on living_count
                            CASE block3x3(1)(1) IS
                                WHEN '1' =>
                                    CASE living_count IS
                                        WHEN 2 | 3 => new_map(new_n)(new_m) <= '1';
                                        WHEN OTHERS => new_map(new_n)(new_m) <= '0';
                                    END CASE;
                                WHEN '0' =>
                                    CASE living_count IS
                                        WHEN 3 => new_map(new_n)(new_m) <= '1';
                                        WHEN OTHERS => new_map(new_n)(new_m) <= '0';
                                    END CASE;
                                WHEN OTHERS => NULL;
                            END CASE;
                        END IF;
                    END LOOP;
                    -- report to_string(new_map(row_count));
                WHEN calculated =>
                    -- NSL
                    IF tick < max_tick - 1 THEN
                        state <= load_row;
                    ELSE
                        state <= init;
                    END IF;

                    -- OFL
                    row_count <= 0;
                    tick <= tick + 1;
                    curr_map <= new_map;

                    FOR i IN 1 TO H LOOP
                        FOR j IN 0 TO W - 1 LOOP
                            row(j) := curr_map(i)(j + 1);
                        END LOOP;
                        REPORT to_string(row);
                        IF (i /= H) THEN
                            write(out_line, row);
                            writeline(out_file, out_line);
                        ELSE
                            write(out_line, row);
                        END IF;
                    END LOOP;
                    REPORT "";
                    write(out_line, lf);
                    writeline(out_file, out_line);
                WHEN OTHERS =>
                    -- NSL
                    state <= init;
            END CASE;
        END IF;
    END PROCESS;
END ARCHITECTURE; -- arch