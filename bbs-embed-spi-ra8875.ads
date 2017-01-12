with Ada.Text_IO;
with Ada.Integer_Text_IO;
--
with BBS.embed.GPIO;
with BBS.embed.SPI;
package BBS.embed.SPI.RA8875 is
   --
   -- Define the object for the RA8875 controller
   --
   type RA8875_record is tagged private;
   type RA8875_ptr is access all RA8875_record;
   --
   -- Enumeration for supported screen sizes
   --
   type RA8875_sizes is (RA8875_480x272, RA8875_800x480);
   --
   -- Constants for RA8875 registers and bits
   --
   -- Power and display control register
   RA8875_PWRR : constant uint8 := 16#01#;
   RA8875_PWRR_DISPON : constant uint8 := 16#80#;
   RA8875_PWRR_DISPOFF : constant uint8 := 16#00#;
   RA8875_PWRR_SLEEP : constant uint8 := 16#02#;
   RA8875_PWRR_NORMAL : constant uint8 := 16#00#;
   RA8875_PWRR_SOFTRESET : constant uint8 := 16#01#;
   --
   -- Memory read/write command
   RA8875_MRWC : constant uint8 := 16#02#;
   --
   -- Pixel clock setting register
   RA8875_PCSR : constant uint8 := 16#04#;
   RA8875_PCSR_PDATR : constant uint8 := 16#00#;
   RA8875_PCSR_PDATL : constant uint8 := 16#80#;
   RA8875_PCSR_CLK : constant uint8 := 16#00#;
   RA8875_PCSR_2CLK : constant uint8 := 16#01#;
   RA8875_PCSR_4CLK : constant uint8 := 16#02#;
   RA8875_PCSR_8CLK : constant uint8 := 16#03#;
   --
   -- Serial flash/ROM configuration register
   RA8875_SROC : constant uint8 := 16#05#;
   --
   -- Serial flash/ROM CLK setting register
   RA8875_SFCLR : constant uint8 := 16#06#;
   --
   -- System configuration register
   RA8875_SYSR : constant uint8 := 16#10#;
   RA8875_SYSR_8BPP : constant uint8 := 16#00#;
   RA8875_SYSR_16BPP : constant uint8 := 16#0C#;
   RA8875_SYSR_MCU8 : constant uint8 := 16#00#;
   RA8875_SYSR_MCU16 : constant uint8 := 16#03#;
   --
   -- General purpose input
   RA8875_GPI : constant uint8 := 16#12#;
   --
   -- General purpose output
   RA8875_GPO : constant uint8 := 16#13#;
   --
   -- Horizontal Display Width Register
   RA8875_HDWR : constant uint8 := 16#14#;
   --
   -- Horizontal non-display fine tuning option register
   RA8875_HNDFTR : constant uint8 := 16#15#;
   RA8875_HNDFTR_DE_HIGH : constant uint8 := 16#00#;
   RA8875_HNDFTR_DE_LOW : constant uint8 := 16#80#;
   --
   -- LCD Horizontal non-display period register
   RA8875_HNDR : constant uint8 := 16#16#;
   --
   -- HSYNC start position register
   RA8875_HSTR : constant uint8 := 16#17#;
   --
   -- HSYNC pulse width register
   --
   RA8875_HPWR : constant uint8 := 16#18#;
   RA8875_HPWR_LOW : constant uint8 := 16#00#;
   RA8875_HPWR_HIGH : constant uint8 := 16#80#;
   --
   -- LCD vertical display height register 0
   RA8875_VDHR0 : constant uint8 := 16#19#;
   --
   -- LCD vertical display height register 1
   RA8875_VDHR1 : constant uint8 := 16#1A#;
   --
   -- LCD vertical non-display period register 0
   RA8875_VNDR0 : constant uint8 := 16#1B#;
   --
   -- LVD vertical non-display period register 1
   RA8875_VNDR1 : constant uint8 := 16#1C#;
   --
   -- VSYNC start position register 0
   RA8875_VSTR0 : constant uint8 := 16#1D#;
   --
   -- VSYNC start position register 1
   RA8875_VSTR1 : constant uint8 := 16#1E#;
   --
   -- VSYNC pulse width register
   RA8875_VPWR : constant uint8 := 16#1F#;
   RA8875_VPWR_LOW : constant uint8 := 16#00#;
   RA8875_VPWR_HIGH : constant uint8 := 16#80#;
   --
   -- Display configuration register
   RA8875_DPCR : constant uint8 := 16#20#;
   --
   -- Font control register 0
   RA8875_FNCR0 : constant uint8 := 16#21#;
   RA8875_FNCR0_CGRAM : constant uint8 := 16#80#;
   RA8875_FNCR0_EXTCR : constant uint8 := 16#20#;
   RA8875_FNCR0_ISO8859_4 : constant uint8 := 16#03#;
   RA8875_FNCR0_ISO8859_3 : constant uint8 := 16#02#;
   RA8875_FNCR0_ISO8859_2 : constant uint8 := 16#01#;
   RA8875_FNCR0_ISO8859_1 : constant uint8 := 16#00#;
   --
   -- Font control register 1
   RA8875_FNCR1 : constant uint8 := 16#22#;
   RA8875_FNCR1_ALIGN : constant uint8 := 16#80#;
   RA8875_FNCR1_TRANS : constant uint8 := 16#40#;
   RA8875_FNCR1_ROT : constant uint8 := 16#10#;
   RA8875_FNCR1_HLARGE : constant uint8 := 16#0C#;
   RA8875_FNCR1_VLARGE : constant uint8 := 16#03#;
   --
   -- CGRAM select register
   RA8875_CGSR : constant uint8 := 16#23#;
   --
   -- Horizontal scroll offset register 0
   RA8875_HOFS0 : constant uint8 := 16#24#;
   --
   -- Horizontal scroll offset register 1
   RA8875_HOFS1 : constant uint8 := 16#25#;
   --
   -- Vertical scroll offset register 0
   RA8875_VOFS0 : constant uint8 := 16#26#;
   --
   -- Vertical scroll offset register 1
   RA8875_VOFS1 : constant uint8 := 16#27#;
   --
   -- Font line distance setting register
   RA8875_FLDR : constant uint8 := 16#29#;
   --
   -- Font write cursor horizontal position register 0
   RA8875_F_CURXL : constant uint8 := 16#2A#;
   --
   -- Font write cursor horizontal position register 1
   RA8875_F_CURXH : constant uint8 := 16#2B#;
   --
   -- Font write cursor vertical position register 0
   RA8875_F_CURYL : constant uint8 := 16#2C#;
   --
   -- Font write cursor vertical position register 1
   RA8875_F_CURYH : constant uint8 := 16#2D#;
   --
   -- Font write type setting register
   RA8875_FWTSR : constant uint8 := 16#2E#;
   --
   -- Serial font ROM setting
   RA8875_SFRS : constant uint8 := 16#2F#;
   --
   -- Horizontal start point 0 of active window
   RA8875_HSAW0 : constant uint8 := 16#30#;
   --
   -- Horizontal start point 1 of active window
   RA8875_HSAW1 : constant uint8 := 16#31#;
   --
   -- Vertical start point 0 of active window
   RA8875_VSAW0 : constant uint8 := 16#32#;
   --
   -- Vertical start point 1 of active window
   RA8875_VSAW1 : constant uint8 := 16#33#;
   --
   -- Horizontal end point 0 of active window
   RA8875_HEAW0 : constant uint8 := 16#34#;
   --
   -- Horizontal end point 1 of active window
   RA8875_HEAW1 : constant uint8 := 16#35#;
   --
   -- Vertical end point 0 of active window
   RA8875_VEAW0 : constant uint8 := 16#36#;
   --
   -- Vertical end point 1 of active window
   RA8875_VEAW1 : constant uint8 := 16#37#;
   --
   -- Horizontal start point 0 of scroll window
   RA8875_HSSW0 : constant uint8 := 16#38#;
   --
   -- Horizontal start point 1 of scroll window
   RA8875_HSSW1 : constant uint8 := 16#39#;
   --
   -- Vertical start point 0 of scroll window
   RA8875_VSSW0 : constant uint8 := 16#3A#;
   --
   -- Vertical start point 1 of scroll window
   RA8875_VSSW1 : constant uint8 := 16#3B#;
   --
   -- Horizontal end point 0 of scroll window
   RA8875_HESW0 : constant uint8 := 16#3C#;
   --
   -- Horizontal end point 1 of scroll window
   RA8875_HESW1 : constant uint8 := 16#3D#;
   --
   -- Vertical end point 0 of scroll window
   RA8875_VESW0 : constant uint8 := 16#3E#;
   --
   -- Vertical end point 1 of scroll window
   RA8875_VESW1 : constant uint8 := 16#3F#;
   --
   -- Memory write control register 0
   RA8875_MWCR0 : constant uint8 := 16#40#;
   RA8875_MWCR0_GFXMODE : constant uint8 := 16#00#;
   RA8875_MWCR0_TXTMODE : constant uint8 := 16#80#;
   --
   -- Memory write control register 1
   RA8875_MWCR1 : constant uint8 := 16#41#;
   --
   -- Blink time control register
   RA8875_BTCR : constant uint8 := 16#44#;
   --
   -- Memory read cursor direction
   RA8875_MRCD : constant uint8 := 16#45#;
   --
   -- Memory write cursor horizontal position register 0
   RA8875_CURH0 : constant uint8 := 16#46#;
   --
   -- Memory write cursor horizontal position register 1
   RA8875_CURH1 : constant uint8 := 16#47#;
   --
   -- Memory write cursor vertical position register 0
   RA8875_CURV0 : constant uint8 := 16#48#;
   --
   -- Memory write cursor vertical position register 1
   RA8875_CURV1 : constant uint8 := 16#49#;
   --
   -- Memory read cursor horizontal position register 0
   RA8875_RCURH0 : constant uint8 := 16#4A#;
   --
   -- Memory read cursor horizontal position register 1
   RA8875_RCURH1 : constant uint8 := 16#4B#;
   --
   -- Memory read cursor vertical position register 0
   RA8875_RCURV0 : constant uint8 := 16#4C#;
   --
   -- Memory read cursor vertical position register 1
   RA8875_RCURV1 : constant uint8 := 16#4D#;
   --
   -- Font write cursor and memory write cursor horizontal size register
   RA8875_CURHS : constant uint8 := 16#4E#;
   --
   -- Font write cursor and memory write cursor vertical size register
   RA8875_CURVS : constant uint8 := 16#4F#;
   --
   -- Block transfer engine (BTE) control register 0
   RA8875_BECR0 : constant uint8 := 16#50#;
   --
   -- Block transfer engine (BTE) control register 1
   RA8875_BECR1 : constant uint8 := 16#51#;
   --
   -- Layer transparency register 0
   RA8875_LTPR0 : constant uint8 := 16#52#;
   --
   -- Layer transparency register 1
   RA8875_LTPR1 : constant uint8 := 16#53#;
   --
   -- Horizontal source point 0 of BTE
   RA8875_HSBE0 : constant uint8 := 16#54#;
   --
   -- Horizontal source point 1 of BTE
   RA8875_HSBE1 : constant uint8 := 16#55#;
   --
   -- Vertical source point 0 of BTE
   RA8875_VSBE0 : constant uint8 := 16#56#;
   --
   -- Vertical source point 1 of BTE
   RA8875_VSBE1 : constant uint8 := 16#57#;
   --
   -- Horizontal destination point 0 of BTE
   RA8875_HDBE0 : constant uint8 := 16#58#;
   --
   -- Horizontal destination point 1 of BTE
   RA8875_HDBE1 : constant uint8 := 16#59#;
   --
   -- Vertical destination point 0 of BTE
   RA8875_VDBE0 : constant uint8 := 16#5A#;
   --
   -- Vertical destination point 1 of BTE
   RA8875_VDBE1 : constant uint8 := 16#5B#;
   --
   -- BTE width register 0
   RA8875_BEWR0 : constant uint8 := 16#5C#;
   --
   -- BTE width register 1
   RA8875_BEWR1 : constant uint8 := 16#5D#;
   --
   -- BTE height register 0
   RA8875_BEHR0 : constant uint8 := 16#5E#;
   --
   -- BTE height register 1
   RA8875_BEHR1 : constant uint8 := 16#5F#;
   --
   -- Background color register 0 (red)
   RA8875_BGCR0 : constant uint8 := 16#60#;
   --
   -- Background color register 1 (green)
   RA8875_BGCR1 : constant uint8 := 16#61#;
   --
   -- Background color register 2 (blue)
   RA8875_BGCR2 : constant uint8 := 16#62#;
   --
   -- Foreground color register 0 (red)
   RA8875_FGCR0 : constant uint8 := 16#63#;
   --
   -- Foreground color register 1 (green)
   RA8875_FGCR1 : constant uint8 := 16#64#;
   --
   -- Foreground color register 2 (blue)
   RA8875_FGCR2 : constant uint8 := 16#65#;
   --
   -- Pattern set no for BTE
   RA8875_PTNO : constant uint8 := 16#66#;
   --
   -- Background color register for transparent 0 (red)
   RA8875_BGTR0 : constant uint8 := 16#67#;
   --
   -- Background color register for transparent 1 (green)
   RA8875_BGTR1 : constant uint8 := 16#68#;
   --
   -- Background color register for transparent 2 (blue)
   RA8875_BGTR2 : constant uint8 := 16#69#;
   --
   -- Touch panel control register 0
   RA8875_TPCR0 : constant uint8 := 16#70#;
   RA8875_TPCR0_ENABLE : constant uint8 := 16#80#;
   RA8875_TPCR0_DISABLE : constant uint8 := 16#00#;
   RA8875_TPCR0_WAIT_512CLK : constant uint8 := 16#00#;
   RA8875_TPCR0_WAIT_1024CLK : constant uint8 := 16#10#;
   RA8875_TPCR0_WAIT_2048CLK : constant uint8 := 16#20#;
   RA8875_TPCR0_WAIT_4096CLK : constant uint8 := 16#30#;
   RA8875_TPCR0_WAIT_8192CLK : constant uint8 := 16#40#;
   RA8875_TPCR0_WAIT_16384CLK : constant uint8 := 16#50#;
   RA8875_TPCR0_WAIT_32768CLK : constant uint8 := 16#60#;
   RA8875_TPCR0_WAIT_65536CLK : constant uint8 := 16#70#;
   RA8875_TPCR0_WAKEENABLE : constant uint8 := 16#08#;
   RA8875_TPCR0_WAKEDISABLE : constant uint8 := 16#00#;
   RA8875_TPCR0_ADCCLK_DIV1 : constant uint8 := 16#00#;
   RA8875_TPCR0_ADCCLK_DIV2 : constant uint8 := 16#01#;
   RA8875_TPCR0_ADCCLK_DIV4 : constant uint8 := 16#02#;
   RA8875_TPCR0_ADCCLK_DIV8 : constant uint8 := 16#03#;
   RA8875_TPCR0_ADCCLK_DIV16 : constant uint8 := 16#04#;
   RA8875_TPCR0_ADCCLK_DIV32 : constant uint8 := 16#05#;
   RA8875_TPCR0_ADCCLK_DIV64 : constant uint8 := 16#06#;
   RA8875_TPCR0_ADCCLK_DIV128 : constant uint8 := 16#07#;
   --
   -- Touch panel control register 1
   RA8875_TPCR1 : constant uint8 := 16#71#;
   RA8875_TPCR1_AUTO : constant uint8 := 16#00#;
   RA8875_TPCR1_MANUAL : constant uint8 := 16#40#;
   RA8875_TPCR1_VREFINT : constant uint8 := 16#00#;
   RA8875_TPCR1_VREFEXT : constant uint8 := 16#20#;
   RA8875_TPCR1_DEBOUNCE : constant uint8 := 16#04#;
   RA8875_TPCR1_NODEBOUNCE : constant uint8 := 16#00#;
   RA8875_TPCR1_IDLE : constant uint8 := 16#00#;
   RA8875_TPCR1_WAIT : constant uint8 := 16#01#;
   RA8875_TPCR1_LATCHX : constant uint8 := 16#02#;
   RA8875_TPCR1_LATCHY : constant uint8 := 16#03#;
   --
   -- Touch panel X high byte data register
   RA8875_TPXH : constant uint8 := 16#72#;
   --
   -- Touch panel Y high byte data register
   RA8875_TPYH : constant uint8 := 16#73#;
   --
   -- Touch panel X/Y low byte data register
   RA8875_TPXYL : constant uint8 := 16#74#;
   RA8875_TPXYL_TOUCHED : constant uint8 := 16#80#;
   RA8875_TPXYL_Y_LSB : constant uint8 := 16#0C#;
   RA8875_TPXYL_X_LSB : constant uint8 := 16#03#;
   --
   -- Graphic cursor horizontal register 0
   RA8875_GCHP0 : constant uint8 := 16#80#;
   --
   -- Graphic cursor horizontal register 1
   RA8875_GCHP1 : constant uint8 := 16#81#;
   --
   -- Graphic cursor vertical register 0
   RA8875_GCHV0 : constant uint8 := 16#82#;
   --
   -- Graphic cursor vertical register 1
   RA8875_GCHV1 : constant uint8 := 16#83#;
   --
   -- Graphics cursor color 0
   RA8875_GCC0 : constant uint8 := 16#84#;
   --
   -- Graphics cursor color 1
   RA8875_GCC1 : constant uint8 := 16#85#;
   --
   -- PLL control register 1
   RA8875_PLLC1 : constant uint8 := 16#88#;
   RA8875_PLLC1_PLLDIV2 : constant uint8 := 16#80#;
   RA8875_PLLC1_PLLDIV1 : constant uint8 := 16#00#;
   --
   -- PLL control register 2
   RA8875_PLLC2 : constant uint8 := 16#89#;
   RA8875_PLLC2_DIV1 : constant uint8 := 16#00#;
   RA8875_PLLC2_DIV2 : constant uint8 := 16#01#;
   RA8875_PLLC2_DIV4 : constant uint8 := 16#02#;
   RA8875_PLLC2_DIV8 : constant uint8 := 16#03#;
   RA8875_PLLC2_DIV16 : constant uint8 := 16#04#;
   RA8875_PLLC2_DIV32 : constant uint8 := 16#05#;
   RA8875_PLLC2_DIV64 : constant uint8 := 16#06#;
   RA8875_PLLC2_DIV128 : constant uint8 := 16#07#;
   --
   -- PWM Control registers.  Note that PWM1 and PWM use the same values, so
   -- there is no point defining separate constants for each.  Only the
   -- addresses are different.
   RA8875_P1CR : constant uint8 := 16#8A#;
   RA8875_P1DCR : constant uint8 := 16#8B#;
   RA8875_P2CR : constant uint8 := 16#8C#;
   RA8875_P2DCR : constant uint8 := 16#8D#;
   --
   -- Constants for PWM CR
   RA8875_PWMCR_ENABLE : constant uint8 := 16#80#;
   RA8875_PWMCR_DISABLE : constant uint8 := 16#00#;
   RA8875_PWMCR_CLKOUT : constant uint8 := 16#10#;
   RA8875_PWMCR_PWMOUT : constant uint8 := 16#00#;
   --
   -- Constants for PWM DCR
   RA8875_PWM_CLK_DIV1 : constant uint8 := 16#00#;
   RA8875_PWM_CLK_DIV2 : constant uint8 := 16#01#;
   RA8875_PWM_CLK_DIV4 : constant uint8 := 16#02#;
   RA8875_PWM_CLK_DIV8 : constant uint8 := 16#03#;
   RA8875_PWM_CLK_DIV16 : constant uint8 := 16#04#;
   RA8875_PWM_CLK_DIV32 : constant uint8 := 16#05#;
   RA8875_PWM_CLK_DIV64 : constant uint8 := 16#06#;
   RA8875_PWM_CLK_DIV128 : constant uint8 := 16#07#;
   RA8875_PWM_CLK_DIV256 : constant uint8 := 16#08#;
   RA8875_PWM_CLK_DIV512 : constant uint8 := 16#09#;
   RA8875_PWM_CLK_DIV1024 : constant uint8 := 16#0A#;
   RA8875_PWM_CLK_DIV2048 : constant uint8 := 16#0B#;
   RA8875_PWM_CLK_DIV4096 : constant uint8 := 16#0C#;
   RA8875_PWM_CLK_DIV8192 : constant uint8 := 16#0D#;
   RA8875_PWM_CLK_DIV16384 : constant uint8 := 16#0E#;
   RA8875_PWM_CLK_DIV32768 : constant uint8 := 16#0F#;
   --
   -- Memory clear register
   RA8875_MCLR : constant uint8 := 16#8E#;
   RA8875_MCLR_START : constant uint8 := 16#80#;
   RA8875_MCLR_STOP : constant uint8 := 16#00#;
   RA8875_MCLR_READSTATUS : constant uint8 := 16#80#;
   RA8875_MCLR_FULL : constant uint8 := 16#00#;
   RA8875_MCLR_ACTIVE : constant uint8 := 16#40#;
   --
   -- Draw line/circle/square control register
   RA8875_DCR : constant uint8 := 16#90#;
   RA8875_DCR_LINESQUTRI_START : constant uint8 := 16#80#;
   RA8875_DCR_LINESQUTRI_STOP : constant uint8 := 16#00#;
   RA8875_DCR_LINESQUTRI_STATUS : constant uint8 := 16#80#;
   RA8875_DCR_CIRCLE_START : constant uint8 := 16#40#;
   RA8875_DCR_CIRCLE_STATUS : constant uint8 := 16#40#;
   RA8875_DCR_CIRCLE_STOP : constant uint8 := 16#00#;
   RA8875_DCR_FILL : constant uint8 := 16#20#;
   RA8875_DCR_NOFILL : constant uint8 := 16#00#;
   RA8875_DCR_DRAWLINE : constant uint8 := 16#00#;
   RA8875_DCR_DRAWTRIANGLE : constant uint8 := 16#01#;
   RA8875_DCR_DRAWSQUARE : constant uint8 := 16#10#;
   --
   -- Draw line/square horizontal start address register 0
   RA8875_DLHSR0 : constant uint8 := 16#91#;
   --
   -- Draw line/square horizontal start address register 1
   RA8875_DLHSR1 : constant uint8 := 16#92#;
   --
   -- Draw line/square vertical start address register 0
   RA8875_DLVSR0 : constant uint8 := 16#93#;
   --
   -- Draw line/square vertical start address register 1
   RA8875_DLVSR1 : constant uint8 := 16#94#;
   --
   -- Draw line/square horizontal end address register 0
   RA8875_DLHER0 : constant uint8 := 16#95#;
   --
   -- Draw line/square horizontal end address register 1
   RA8875_DLHER1 : constant uint8 := 16#96#;
   --
   -- Draw line/square vertical end address register 0
   RA8875_DLVER0 : constant uint8 := 16#97#;
   --
   -- Draw line/square vertical end address register 1
   RA8875_DLVER1 : constant uint8 := 16#98#;
   --
   -- Draw circle center horizontal address register 0
   RA8875_DCHR0 : constant uint8 := 16#99#;
   --
   -- Draw circle center horizontal address register 1
   RA8875_DCHR1 : constant uint8 := 16#9A#;
   --
   -- Draw circle center vertical address register 0
   RA8875_DCHV0 : constant uint8 := 16#9B#;
   --
   -- Draw circle center vertical address register 1
   RA8875_DCHV1 : constant uint8 := 16#9C#;
   --
   -- Draw circle radius register
   RA8875_DCRR : constant uint8 := 16#9D#;
   --
   -- Draw ellipse/ellipse curve/circle square control register
   RA8875_ELLIPSE : constant uint8 := 16#A0#;
   RA8875_ELLIPSE_STATUS : constant uint8 := 16#80#;
   RA8875_ELLIPSE_START : constant uint8 := 16#80#;
   RA8875_ELLIPSE_FILL : constant uint8 := 16#40#;
   RA8875_ELLIPSE_SQR : constant uint8 := 16#20#;
   --
   -- Draw ellipse/circle square long axis setting register 0
   RA8875_ELL_A0 : constant uint8 := 16#A1#;
   --
   -- Draw ellipse/circle square long axis setting register 1
   RA8875_ELL_A1 : constant uint8 := 16#A2#;
   --
   -- Draw ellipse/circle square short axis setting register 0
   RA8875_ELL_B0 : constant uint8 := 16#A3#;
   --
   -- Draw ellipse/circle square short axis setting register 1
   RA8875_ELL_B1 : constant uint8 := 16#A4#;
   --
   -- Draw ellipse/circle square center horizontal address register 0
   RA8875_DEHR0 : constant uint8 := 16#A5#;
   --
   -- Draw ellipse/circle square center horizontal address register 1
   RA8875_DEHR1 : constant uint8 := 16#A6#;
   --
   -- Draw ellipse/circle square center vertical address register 0
   RA8875_DEVR0 : constant uint8 := 16#A7#;
   --
   -- Draw ellipse/circle square center vertical address register 1
   RA8875_DEVR1 : constant uint8 := 16#A8#;
   --
   -- Draw triangle point 2 horizontal address register 0
   RA8875_DTPH0 : constant uint8 := 16#A9#;
   --
   -- Draw triangle point 2 horizontal address register 1
   RA8875_DTPH1 : constant uint8 := 16#AA#;
   --
   -- Draw triangle point 2 vertical address register 0
   RA8875_DTPV0 : constant uint8 := 16#AB#;
   --
   -- Draw triangle point 2 vertical address register 1
   RA8875_DTPV1 : constant uint8 := 16#AC#;
   --
   -- DMA Source starting address reg 0
   RA8875_SSAR0 : constant uint8 := 16#B0#;
   --
   -- DMA Source starting address reg 1
   RA8875_SSAR1 : constant uint8 := 16#B1#;
   --
   -- DMA Source starting address reg 2
   RA8875_SSAR2 : constant uint8 := 16#B2#;
   --
   -- DMA Block width register 0 / DMA transfer number register 0
   RA8875_DTNR0 : constant uint8 := 16#B4#;
   --
   -- DMA Block width register 1
   RA8875_BWR1 : constant uint8 := 16#B5#;
   --
   -- DMA Block height register 0 / DMA transfer number register 1
   RA8875_DTNR1 : constant uint8 := 16#B6#;
   --
   -- DMA Block height register 1
   RA8875_BHR1 : constant uint8 := 16#B7#;
   --
   -- DMA source picture width register 0 / DMA transfer number register 2
   RA8875_DNTR2 : constant uint8 := 16#B8#;
   --
   -- DMA Source picture register 1
   RA8875_SPWR1 : constant uint8 := 16#B9#;
   --
   -- DMA configuration register
   RA8875_DMACR : constant uint8 := 16#BF#;
   --
   -- Key-Scan control register 1
   RA8875_KSCR1 : constant uint8 := 16#C0#;
   --
   -- Key-Scan control register 2
   RA8875_KSCR2 : constant uint8 := 16#C1#;
   --
   -- Key-Scan data register 0
   RA8875_KSRD0 : constant uint8 := 16#C2#;
   --
   -- Key-Scan data register 1
   RA8875_KSRD1 : constant uint8 := 16#C3#;
   --
   -- Key-Scan data register 2
   RA8875_KSRD2 : constant uint8 := 16#C4#;
   --
   -- Extra general purpose I/O register
   RA8875_GPIOX : constant uint8 := 16#C7#;
   --
   -- Floating window start address XA 0
   RA8875_FWSAXA0 : constant uint8 := 16#D0#;
   --
   -- Floating window start address XA 1
   RA8875_FWSAXA1 : constant uint8 := 16#D1#;
   --
   -- Floating window start address YA 0
   RA8875_FWSAYA0 : constant uint8 := 16#D2#;
   --
   -- Floating window start address YA 1
   RA8875_FWSAYA1 : constant uint8 := 16#D3#;
   --
   -- Floating window width 0
   RA8875_FWW0 : constant uint8 := 16#D4#;
   --
   -- Floating window width 1
   RA8875_FWW1 : constant uint8 := 16#D5#;
   --
   -- Floating window height 0
   RA8875_FWH0 : constant uint8 := 16#D6#;
   --
   -- Floating window height 1
   RA8875_FWH1 : constant uint8 := 16#D7#;
   --
   -- Floating window display X address 0
   RA8875_FWDXA0 : constant uint8 := 16#D8#;
   --
   -- Floating window display X address 1
   RA8875_FWDXA1 : constant uint8 := 16#D9#;
   --
   -- Floating window display Y address 0
   RA8875_FWDYA0 : constant uint8 := 16#DA#;
   --
   -- Floating window display Y address 1
   RA8875_FWDYA1 : constant uint8 := 16#DB#;
   --
   -- Serial flash/ROM direct access mode
   RA8875_SACS_MODE : constant uint8 := 16#E0#;
   --
   -- Serial flash/ROM direct access mode address
   RA8875_SACS_ADDR : constant uint8 := 16#E1#;
   --
   -- Serial flash/ROM direct access data read
   RA8875_SACS_DATA : constant uint8 := 16#E2#;
   --
   -- Interrupt control register 1
   RA8875_INTC1 : constant uint8 := 16#F0#;
   RA8875_INTC1_KEY : constant uint8 := 16#10#;
   RA8875_INTC1_DMA : constant uint8 := 16#08#;
   RA8875_INTC1_TP : constant uint8 := 16#04#;
   RA8875_INTC1_BTE : constant uint8 := 16#02#;
   --
   -- Interrupt control register 2
   RA8875_INTC2 : constant uint8 := 16#F1#;
   RA8875_INTC2_KEY : constant uint8 := 16#10#;
   RA8875_INTC2_DMA : constant uint8 := 16#08#;
   RA8875_INTC2_TP : constant uint8 := 16#04#;
   RA8875_INTC2_BTE : constant uint8 := 16#02#;
   --
   -- Colors (RGB565)
   --
   type R5G6B5_color is record
      R : uint8 range 0 .. 31;
      G : uint8 range 0 .. 63;
      B : uint8 range 0 .. 31;
end record;
--
-- To match the 16 bit definition, add the following:
--
--     with pack, size => 16;
--   for R5G6B5_color use
--      record
--         B at 0 range 0 .. 4;
--         G at 0 range 5 .. 10;
--         R at 0 range 11 .. 15;
--      end record;
   --
   -- Define some common colors
   --
   RA8875_BLACK : constant R5G6B5_color := (R => 0, G => 0, B => 0);
   RA8875_BLUE : constant R5G6B5_color := (R => 0, G => 0, B => 31);
   RA8875_RED : constant R5G6B5_color := (R => 31, G => 0, B => 0);
   RA8875_GREEN : constant R5G6B5_color := (R => 0, G => 63, B => 0);
   RA8875_CYAN : constant R5G6B5_color := (R => 0, G => 63, B => 31);
   RA8875_MAGENTA : constant R5G6B5_color := (R => 31, G => 0, B => 31);
   RA8875_YELLOW : constant R5G6B5_color := (R => 31, G => 63, B => 0);
   RA8875_WHITE : constant R5G6B5_color := (R => 31, G => 63, B => 31);
   --
   function RA8875_new return RA8875_ptr;
   --
   -- Low level methods
   --
   procedure setup(self : in out RA8875_record; CS : GPIO.GPIO; screen : SPI_ptr);
   procedure writeCmd(self : RA8875_record; value : uint8);
   procedure writeData(self : RA8875_record; value : uint8);
   function readStatus(self : RA8875_record) return uint8;
   function readData(self : RA8875_record) return uint8;
   procedure writeReg(self : RA8875_record; reg : uint8; value : uint8);
   function readReg(self : RA8875_record; reg : uint8) return uint8;
   --
   -- Configuration methods
   --
   procedure configure(self : in out RA8875_record; size : RA8875_sizes);
   procedure set_sleep(self : RA8875_record; state : boolean);
   procedure set_display(self : RA8875_record; state : boolean);
   procedure GPIOX(self : RA8875_record; state : boolean);
   procedure PWM1config(self : RA8875_record; state : boolean; clock : uint8);
   procedure PWM2config(self : RA8875_record; state : boolean; clock : uint8);
   procedure PWM1out(self : RA8875_record; value : uint8);
   procedure PWM2out(self : RA8875_record; value : uint8);
   --
   -- Text methods
   --
   procedure textMode(self : RA8875_record);
   procedure textColor(self : RA8875_record; bg : R5G6B5_color; fg : R5G6B5_color);
   procedure textSetCursor(self : RA8875_record; x : uint16; y : uint16);
   procedure textSetCodePage(self : RA8875_record; page : uint8);
   procedure textSetAttribute(self : RA8875_record; align : boolean; transparent : boolean;
                              rotate : boolean; h_size : uint8; v_size : uint8);
   procedure textWrite(self : RA8875_record; str : string);
   --
   -- Graphics methods
   --
   procedure graphicsMode(self : RA8875_record);
   procedure drawRect(self : RA8875_record; x : uint16; y : uint16; w : uint16;
                      h : uint16; color : R5G6B5_color; fill : boolean);
   procedure drawRndRect(self : RA8875_record; x : uint16; y : uint16; w : uint16;
                      h : uint16; rad : uint16; color : R5G6B5_color; fill : boolean);
   procedure drawLine(self : RA8875_record; x : uint16; y : uint16; w : uint16;
                      h : uint16; color : R5G6B5_color);
   procedure waitPoll(self : RA8875_record; reg : uint8; flag : uint8);
   --
   -- Touch methods
   --
   procedure enableTouch(self : RA8875_record; state : boolean);
   function checkTouched(self : RA8875_record) return boolean;
   procedure readTouch(self : RA8875_record; x : out uint16; y : out uint16);
--
private
   type RA8875_record is tagged
      record
         cs_gpio : BBS.embed.GPIO.GPIO;
         reset_gpio : BBS.embed.GPIO.GPIO;
         lcd_screen : BBS.embed.SPI.SPI_ptr;
         width : uint16;
         height : uint16;
      end record;
   --
   -- Constants for pin outputs
   --
   cs_high : constant bit := 0;
   cs_low : constant bit := 1;
   --
   -- Constants for RA8875 transaction types
   --
   RA8875_DATAWRITE : constant uint8 := 16#00#;
   RA8875_DATAREAD : constant uint8 := 16#40#;
   RA8875_CMDWRITE : constant uint8 := 16#80#;
   RA8875_CMDREAD : constant uint8 := 16#C0#;

end;
