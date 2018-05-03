LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY lichtenstein IS
      PORT(
			-- 24MHz main clock
			clkin:		  IN std_logic;
		
			-- SRAM bus
			sram_data: INOUT std_logic_vector(7 downto 0);
			sram_addr:   OUT std_logic_vector(17 downto 0);
			sram_oe:		 OUT std_logic;
			sram_ce:		 OUT std_logic;
			sram_we:		 OUT std_logic;
			
			-- SPI bus
			spi_cs:		  IN std_logic;
			spi_mosi:	  IN std_logic;
			spi_miso:	 OUT std_logic;
			spi_sck:		  IN std_logic;
			
			-- strobes from host
			ledout_en:	  IN std_logic;
			ledout_rst:	  IN std_logic;
			
			-- status outputs
			status:		 OUT std_logic_vector(2 downto 0);
			
			-- PWM output
			pwm_out:		 OUT std_logic_vector(15 downto 0)
		);
END lichtenstein;

ARCHITECTURE a OF lichtenstein IS

BEGIN

END a;