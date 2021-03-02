LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY game_of_life_tb IS
END ENTITY;

ARCHITECTURE arch OF game_of_life_tb IS
    COMPONENT game_of_life IS
    PORT (
        clk : IN STD_LOGIC := '0';
        start : IN STD_LOGIC := '0'
    );
    END COMPONENT;

    CONSTANT clk_period : TIME := 10 ns;
    SIGNAL clk : STD_LOGIC := '0';
    signal start : std_logic := '0';
    
BEGIN
    uut : game_of_life 
    PORT MAP(
        clk => clk,
        start => start
    );

    clk_gen : PROCESS
    BEGIN
        FOR i IN 1 TO 50000 LOOP
            clk <= '0';
            WAIT FOR clk_period/2;
            clk <= '1';
            WAIT FOR clk_period/2;
        END LOOP;
        WAIT;
    END PROCESS; -- stim

    stim : PROCESS
    BEGIN
        wait for 100 ns;

        start <= '1';   
        wait for 5 * clk_period;
        start <= '0';
        wait;
    END PROCESS;
END ARCHITECTURE; -- arch