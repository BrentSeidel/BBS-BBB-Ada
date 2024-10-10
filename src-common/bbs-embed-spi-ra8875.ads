--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed.GPIO;
use type BBS.embed.GPIO.GPIO;
with BBS.embed.SPI;
package BBS.embed.SPI.RA8875 is
   ----------------------------------------------------------------------------
   --  Define the object for the RA8875 controller
   --
   type RA8875_record is tagged private;
   type RA8875_ptr is access all RA8875_record;
   --
   --  Enumeration for supported screen sizes
   --
   --  Sizes supported by the RA8875 are 320x240, 320x480, 480x272, 640x480,
   --  and 800x480.
   --  Right now I only have an 800x480 panel for testing so nothing is tested
   --  for other sizes.
   type RA8875_sizes is (RA8875_480x272, RA8875_800x480);
   ----------------------------------------------------------------------------
   -- Type definitions
   --
   --  Used for registers
   type RA8875_LAYER is (LAYER1, LAYER2);
   type FNCR0_Code_Page is (FNCR0_ISO8859_1, FNCR0_ISO8859_2,
                            FNCR0_ISO8859_3, FNCR0_ISO8859_4);
   type MWCR0_MODE is (graphic, text);
   type MWCR0_CURDIR is (LRTD, RLTD, TDLR, DTLR);
   type MWCR1_GCURS_ENABLE is (disable, enable);
   type MWCR1_GCURS_SET is range 0 .. 7;
   type MWCR1_WRITE_DEST is (LAYER, CGRAM, GCURS, PATTERN);
   type LTPR0_SCROLL_MODE is (LAYER12_SIMULTANEOUS, LAYER1_ONLY,
                              LAYER2_ONLY, BUFFERED);
   type LTPR0_DISP_MODE is (ONLY_LAYER1, ONLY_LAYER2, LIGHTEN, TRANSPARENT,
                            BOOL_OR, BOOL_AND, FLOATING, RESERVED);
   type ELLIPSE_PART is (ELLIPSE_LL, ELLIPSE_UL, ELLIPSE_UR, ELLIPSE_LR);
   --
   -- Colors (RGB565)
   --
   type R5G6B5_color is record
      R : uint8 range 0 .. 31;
      G : uint8 range 0 .. 63;
      B : uint8 range 0 .. 31;
   end record;
   --
   --  To match the 16 bit definition, add the following:
   --
   for R5G6B5_color use record
      B at 0 range 0 .. 4;
      G at 0 range 5 .. 10;
      R at 0 range 11 .. 15;
   end record;
   for R5G6B5_color'Size use 16;
   --
   --  Define some common colors
   --
   R5G6B5_BLACK   : constant R5G6B5_color := (R => 0, G => 0, B => 0);
   R5G6B5_BLUE    : constant R5G6B5_color := (R => 0, G => 0, B => 31);
   R5G6B5_RED     : constant R5G6B5_color := (R => 31, G => 0, B => 0);
   R5G6B5_GREEN   : constant R5G6B5_color := (R => 0, G => 63, B => 0);
   R5G6B5_CYAN    : constant R5G6B5_color := (R => 0, G => 63, B => 31);
   R5G6B5_MAGENTA : constant R5G6B5_color := (R => 31, G => 0, B => 31);
   R5G6B5_YELLOW  : constant R5G6B5_color := (R => 31, G => 63, B => 0);
   R5G6B5_WHITE   : constant R5G6B5_color := (R => 31, G => 63, B => 31);
   --
   --  Colors (RGB332)
   --
   type R3G3B2_color is record
      R : uint8 range 0 .. 7;
      G : uint8 range 0 .. 7;
      B : uint8 range 0 .. 3;
   end record with pack, size => 8;
   --
   --  To match the 8 bit definition, add the following:
   --
   for R3G3B2_color use record
      B at 0 range 0 .. 1;
      G at 0 range 2 .. 4;
      R at 0 range 5 .. 7;
   end record;
   function R3G3B2_to_uint8 is new Ada.Unchecked_Conversion(Source => R3G3B2_color,
                                                            Target => uint8);
   function uint8_to_R3G3B2 is new Ada.Unchecked_Conversion(Source => uint8,
                                                            Target => R3G3B2_color);
   --
   --  Define some common colors
   --
   R3G3B2_BLACK   : constant R3G3B2_color := (R => 0, G => 0, B => 0);
   R3G3B2_BLUE    : constant R3G3B2_color := (R => 0, G => 0, B => 3);
   R3G3B2_RED     : constant R3G3B2_color := (R => 7, G => 0, B => 0);
   R3G3B2_GREEN   : constant R3G3B2_color := (R => 0, G => 7, B => 0);
   R3G3B2_CYAN    : constant R3G3B2_color := (R => 0, G => 7, B => 3);
   R3G3B2_MAGENTA : constant R3G3B2_color := (R => 7, G => 0, B => 3);
   R3G3B2_YELLOW  : constant R3G3B2_color := (R => 7, G => 7, B => 0);
   R3G3B2_WHITE   : constant R3G3B2_color := (R => 7, G => 7, B => 3);
   --
   --  Graphics cursor
   --
   --  Cursor pixel value:
   --  0 - GCC0 color
   --  1 - GCC1 color
   --  2 - Background color
   --  3 - Inverse of background color
   --
   --  Note that the coordinates for GCursor are reversed from what one would
   --  expect.  The Y axis coordinate is the first array index and the X axis
   --  coordinate is the second array index.  One can think of it as being in
   --  row, column order.
   --
   type RA8875_GCursor is array (0 .. 31, 0 .. 31) of integer range 0 .. 3
     with Pack;
   type RA8875_GCursorBuffer is array (0 .. 255) of uint8
      with Pack;
   function GCursor_to_buffer is new Ada.Unchecked_Conversion(source => RA8875_GCursor,
                                                              target => RA8875_GCursorBuffer);
   ----------------------------------------------------------------------------
   --
   -- Low level methods
   --
   procedure setup(self : in out RA8875_record; CS : GPIO.GPIO; screen : SPI_ptr);
   procedure setup(self : in out RA8875_record; CS : GPIO.GPIO; RST : GPIO.GPIO; screen : SPI_ptr);
   procedure hwReset(self : in out RA8875_record);
   procedure swReset(self : in out RA8875_record);
   --
   -- Configuration methods
   --
   procedure configure(self : in out RA8875_record; size : RA8875_sizes);
   procedure setSleep(self : RA8875_record; state : boolean);
   procedure setDisplay(self : RA8875_record; state : boolean);
   procedure set_GPIOX(self : RA8875_record; state : boolean);
   procedure PWM1config(self : RA8875_record; state : boolean; clock : uint8);
   procedure PWM2config(self : RA8875_record; state : boolean; clock : uint8);
   procedure PWM1out(self : RA8875_record; value : uint8);
   procedure PWM2out(self : RA8875_record; value : uint8);
   procedure setDisplayCtrl(self : RA8875_record; layer : uint8; hdir : uint8;
                            vdir : uint8);
   procedure setWriteCtrl0(self : RA8875_record; mode : MWCR0_MODE; cursorVisible : boolean;
                           cursorBlink : boolean; writeDir : MWCR0_CURDIR; WriteCursorIncr : boolean;
                           ReadCursorIncr : boolean);
   procedure setWriteCtrl1(self : RA8875_record; cursorEnable : MWCR1_GCURS_ENABLE;
                           GCursorSelect : MWCR1_GCURS_SET; writeDest : MWCR1_WRITE_DEST;
                           layer : RA8875_LAYER);
   --
   -- Text methods
   --
   procedure textMode(self : RA8875_record);
   procedure textColor(self : RA8875_record; bg : R5G6B5_color; fg : R5G6B5_color);
   procedure textSetCodePage(self : RA8875_record; page : FNCR0_Code_Page);
   procedure textSetAttribute(self : RA8875_record; align : boolean; transparent : boolean;
                              rotate : boolean; h_size : uint8; v_size : uint8);
   procedure textSetLineHeight(self : RA8875_record; size : uint8);
   procedure textSetFontWidth(self : RA8875_record; size : uint8);
   procedure textWrite(self : RA8875_record; str : string);
   --
   -- Graphics methods
   --
   procedure graphicsMode(self : RA8875_record);
   -- Set the color for drawing
   procedure drawColor(self : RA8875_record; color : R5G6B5_color);
   -- Use these procedures to draw in whatever color has already been set
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; fill : boolean);
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                         y2 : uint16; rad : uint16; fill : boolean);
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16);
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        fill : boolean);
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          fill : boolean);
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                        vRad : uint16; fill : boolean);
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                                vRad : uint16; seg : ELLIPSE_PART; fill : boolean);
   -- Use these procedures to specify the color to draw
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color; fill : boolean);
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; rad : uint16; color : R5G6B5_color; fill : boolean);
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color);
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        color : R5G6B5_color; fill : boolean);
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          color : R5G6B5_color; fill : boolean);
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                        vRad : uint16; color : R5G6B5_color; fill : boolean);
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                                vRad : uint16; seg : ELLIPSE_PART; color : R5G6B5_color; fill : boolean);
   --
   procedure waitPoll(self : RA8875_record; reg : uint8; flag : uint8);
   --
   -- Touch methods
   --
   procedure enableTouch(self : RA8875_record; state : boolean);
   function checkTouched(self : RA8875_record) return boolean;
   procedure readTouchRaw(self : RA8875_record; x : out uint16; y : out uint16);
   procedure readTouchCal(self : RA8875_record; x : out uint16; y : out uint16);
   procedure touchCalibrate(self : in out RA8875_record);
   procedure setTouchCalibration(self : in out RA8875_record; top : uint16;
                                 bottom : uint16; left : uint16; right : uint16);
   procedure getTouchCalibration(self : RA8875_record; top : out uint16;
                                 bottom : out uint16; left : out uint16; right : out uint16);
   --
   -- Region and layer methods
   --
   procedure scroll(self : RA8875_record; hStart : uint16; vStart : uint16;
                    hEnd : uint16; vEnd : uint16; hOffset : uint16; vOffset : uint16);
   procedure setActiveWindow(self : RA8875_record; top : uint16; bottom : uint16;
                             left : uint16; right : uint16);
   procedure screenActive(self : RA8875_record);
   procedure setLayers(self : RA8875_record; layer : RA8875_LAYER);
   procedure selectLayer(self : RA8875_record; layer : RA8875_LAYER);
   procedure setLayerSetting0(self : RA8875_record; scroll : LTPR0_SCROLL_MODE;
                              float : boolean; display : LTPR0_DISP_MODE);
   --
   -- Cursor methods
   --
   procedure setTextCursorPos(self : RA8875_record; x : uint16; y : uint16);
   procedure setGraphCursorColors(self : RA8875_record; color0 : R3G3B2_color;
                                  color1 : R3G3B2_color);
   procedure setGraphCursorPos(self : RA8875_record; x : uint16; y : uint16);
   procedure setGraphCursor(self : RA8875_record; curs : MWCR1_GCURS_SET;
                            data : RA8875_GCursor);
   procedure selectGraphCursor(self : RA8875_record; curs : MWCR1_GCURS_SET;
                               enable : MWCR1_GCURS_ENABLE);
   --
   -- Miscellaneous methods
   --
   procedure fillScreen(self : RA8875_record; color : R5G6B5_color);
   --
private
   --
   --  Private definitions
   --
   ----------------------------------------------------------------------------
   --  RA8875 Register definitions.  Note that unused registers/definitions
   --  are commented out.
   --
   PWRR           : constant uint8 := 16#01#;   --  Power and display control register
   PWRR_DISPON    : constant uint8 := 16#80#;
   PWRR_DISPOFF   : constant uint8 := 16#00#;
   PWRR_SLEEP     : constant uint8 := 16#02#;
   PWRR_NORMAL    : constant uint8 := 16#00#;
   PWRR_SOFTRESET : constant uint8 := 16#01#;
   --
   MRWC : constant uint8 := 16#02#;   --  Memory read/write command
   --
   PCSR       : constant uint8 := 16#04#;   --  Pixel clock setting register
--   PCSR_PDATR : constant uint8 := 16#00#;
   PCSR_PDATL : constant uint8 := 16#80#;
--   PCSR_CLK   : constant uint8 := 16#00#;
   PCSR_2CLK  : constant uint8 := 16#01#;
   PCSR_4CLK  : constant uint8 := 16#02#;
--   PCSR_8CLK  : constant uint8 := 16#03#;
   --
--   SROC  : constant uint8 := 16#05#;   --  Serial flash/ROM configuration register
--   SFCLR : constant uint8 := 16#06#;   --  Serial flash/ROM CLK setting register
   --
   SYSR : constant uint8 := 16#10#;   --  System configuration register
   SYSR_8BPP  : constant uint8 := 16#00#;
   SYSR_16BPP : constant uint8 := 16#0C#;
   SYSR_MCU8  : constant uint8 := 16#00#;
--   SYSR_MCU16 : constant uint8 := 16#03#;
   --
--   GPI  : constant uint8 := 16#12#;   --  General purpose input
--   GPO  : constant uint8 := 16#13#;   --  General purpose output
   HDWR : constant uint8 := 16#14#;   --  Horizontal Display Width Register
   --
   HNDFTR : constant uint8 := 16#15#;   --  Horizontal non-display fine tuning option register
   HNDFTR_DE_HIGH : constant uint8 := 16#00#;
--   HNDFTR_DE_LOW  : constant uint8 := 16#80#;
   --
   HNDR : constant uint8 := 16#16#;   --  LCD Horizontal non-display period register
   HSTR : constant uint8 := 16#17#;   --  HSYNC start position register
   --
   HPWR : constant uint8 := 16#18#;   --  HSYNC pulse width register
   HPWR_LOW  : constant uint8 := 16#00#;
--   HPWR_HIGH : constant uint8 := 16#80#;
   --
   VDHR0 : constant uint8 := 16#19#;   --  LCD vertical display height register 0
   VDHR1 : constant uint8 := 16#1A#;   --  LCD vertical display height register 1
   VNDR0 : constant uint8 := 16#1B#;   --  LCD vertical non-display period register 0
   VNDR1 : constant uint8 := 16#1C#;   --  LVD vertical non-display period register 1
   VSTR0 : constant uint8 := 16#1D#;   --  VSYNC start position register 0
   VSTR1 : constant uint8 := 16#1E#;   --  VSYNC start position register 1
   --
   VPWR : constant uint8 := 16#1F#;   --  VSYNC pulse width register
   VPWR_LOW  : constant uint8 := 16#00#;
--   VPWR_HIGH : constant uint8 := 16#80#;
   --
   DPCR : constant uint8 := 16#20#;   --  Display configuration register
   DPCR_1LAYER : constant uint8 := 16#00#;
   DPCR_2LAYER : constant uint8 := 16#80#;
--   DPCR_HDIR0  : constant uint8 := 16#00#;
--   DPCR_HDIR1  : constant uint8 := 16#08#;
--   DPCR_VDIR0  : constant uint8 := 16#00#;
--   DPCR_VDIR1  : constant uint8 := 16#04#;
   --
   FNCR0 : constant uint8 := 16#21#;   --  Font control register 0
   FNCR0_CGRAM : constant uint8 := 16#80#;
   FNCR0_EXTCR : constant uint8 := 16#20#;
   --
   FNCR1 : constant uint8 := 16#22#;   --  Font control register 1
   FNCR1_ALIGN  : constant uint8 := 16#80#;
   FNCR1_TRANS  : constant uint8 := 16#40#;
   FNCR1_ROT    : constant uint8 := 16#10#;
--   FNCR1_HLARGE : constant uint8 := 16#0C#;
--   FNCR1_VLARGE : constant uint8 := 16#03#;
   --
--   CGSR    : constant uint8 := 16#23#;   --  CGRAM select register
   HOFS0   : constant uint8 := 16#24#;   --  Horizontal scroll offset register 0
   HOFS1   : constant uint8 := 16#25#;   --  Horizontal scroll offset register 1
   VOFS0   : constant uint8 := 16#26#;   --  Vertical scroll offset register 0
   VOFS1   : constant uint8 := 16#27#;   --  Vertical scroll offset register 1
   FLDR    : constant uint8 := 16#29#;   --  Font line distance setting register
   F_CURXL : constant uint8 := 16#2A#;   --  Font write cursor horizontal position register 0
   F_CURXH : constant uint8 := 16#2B#;   --  Font write cursor horizontal position register 1
   F_CURYL : constant uint8 := 16#2C#;   --  Font write cursor vertical position register 0
   F_CURYH : constant uint8 := 16#2D#;   --  Font write cursor vertical position register 1
   FWTSR   : constant uint8 := 16#2E#;   --  Font write type setting register
--   SFRS    : constant uint8 := 16#2F#;   --  Serial font ROM setting
   HSAW0   : constant uint8 := 16#30#;   --  Horizontal start point 0 of active window
   HSAW1   : constant uint8 := 16#31#;   --  Horizontal start point 1 of active window
   VSAW0   : constant uint8 := 16#32#;   --  Vertical start point 0 of active window
   VSAW1   : constant uint8 := 16#33#;   --  Vertical start point 1 of active window
   HEAW0   : constant uint8 := 16#34#;   --  Horizontal end point 0 of active window
   HEAW1   : constant uint8 := 16#35#;   --  Horizontal end point 1 of active window
   VEAW0   : constant uint8 := 16#36#;   --  Vertical end point 0 of active window
   VEAW1   : constant uint8 := 16#37#;   --  Vertical end point 1 of active window
   HSSW0   : constant uint8 := 16#38#;   --  Horizontal start point 0 of scroll window
   HSSW1   : constant uint8 := 16#39#;   --  Horizontal start point 1 of scroll window
   VSSW0   : constant uint8 := 16#3A#;   --  Vertical start point 0 of scroll window
   VSSW1   : constant uint8 := 16#3B#;   --  Vertical start point 1 of scroll window
--   HESW0   : constant uint8 := 16#3C#;   --  Horizontal end point 0 of scroll window
--   HESW1   : constant uint8 := 16#3D#;   --  Horizontal end point 1 of scroll window
--   VESW0   : constant uint8 := 16#3E#;   --  Vertical end point 0 of scroll window
--   VESW1   : constant uint8 := 16#3F#;   --  Vertical end point 1 of scroll window
   --
   MWCR0   : constant uint8 := 16#40#;   --  Memory write control register 0
--   MWCR0_GFXMODE  : constant uint8 := 16#00#;
   MWCR0_TXTMODE  : constant uint8 := 16#80#;
   MWCR0_CURVIS   : constant uint8 := 16#40#;
   MWCR0_CURBLINK : constant uint8 := 16#20#;
--   MWCR0_CURDIR_SCALE : constant uint8 := 16#04#;
   MWCR0_WRITE_NOINCR : constant uint8 := 16#02#;
   MWCR0_READ_NOINCR  : constant uint8 := 16#01#;
   --
   MWCR1 : constant uint8 := 16#41#;   --  Memory write control register 1
   MWCR1_GCURS_EN        : constant uint8 := 16#80#;
   MWCR1_CUR_SEL_SCALE   : constant uint8 := 16#10#;
   MWCR1_WRITE_DEST_SCALE : constant uint8 := 16#04#;
   --
--   BTCR   : constant uint8 := 16#44#;   --  Blink time control register
--   MRCD   : constant uint8 := 16#45#;   --  Memory read cursor direction
--   CURH0  : constant uint8 := 16#46#;   --  Memory write cursor horizontal position register 0
--   CURH1  : constant uint8 := 16#47#;   --  Memory write cursor horizontal position register 1
--   CURV0  : constant uint8 := 16#48#;   --  Memory write cursor vertical position register 0
--   CURV1  : constant uint8 := 16#49#;   --  Memory write cursor vertical position register 1
--   RCURH0 : constant uint8 := 16#4A#;   --  Memory read cursor horizontal position register 0
--   RCURH1 : constant uint8 := 16#4B#;   --  Memory read cursor horizontal position register 1
--   RCURV0 : constant uint8 := 16#4C#;   --  Memory read cursor vertical position register 0
--   RCURV1 : constant uint8 := 16#4D#;   --  Memory read cursor vertical position register 1
--   CURHS  : constant uint8 := 16#4E#;   --  Font write cursor and memory write cursor horizontal size register
--   CURVS  : constant uint8 := 16#4F#;   --  Font write cursor and memory write cursor vertical size register
--   BECR0  : constant uint8 := 16#50#;   --  Block transfer engine (BTE) control register 0
--   BECR1  : constant uint8 := 16#51#;   --  Block transfer engine (BTE) control register 1
   --
--   LTPR1 : constant uint8 := 16#53#;   --  Layer transparency register 1
--   HSBE0 : constant uint8 := 16#54#;   --  Horizontal source point 0 of BTE
--   HSBE1 : constant uint8 := 16#55#;   --  Horizontal source point 1 of BTE
--   VSBE0 : constant uint8 := 16#56#;   --  Vertical source point 0 of BTE
--   VSBE1 : constant uint8 := 16#57#;   --  Vertical source point 1 of BTE
--   HDBE0 : constant uint8 := 16#58#;   --  Horizontal destination point 0 of BTE
--   HDBE1 : constant uint8 := 16#59#;   --  Horizontal destination point 1 of BTE
--   VDBE0 : constant uint8 := 16#5A#;   --  Vertical destination point 0 of BTE
--   VDBE1 : constant uint8 := 16#5B#;   --  Vertical destination point 1 of BTE
--   BEWR0 : constant uint8 := 16#5C#;   --  BTE width register 0
--   BEWR1 : constant uint8 := 16#5D#;   --  BTE width register 1
--   BEHR0 : constant uint8 := 16#5E#;   --  BTE height register 0
--   BEHR1 : constant uint8 := 16#5F#;   --  BTE height register 1
   BGCR0 : constant uint8 := 16#60#;   --  Background color register 0 (red)
   BGCR1 : constant uint8 := 16#61#;   --  Background color register 1 (green)
   BGCR2 : constant uint8 := 16#62#;   --  Background color register 2 (blue)
   FGCR0 : constant uint8 := 16#63#;   --  Foreground color register 0 (red)
   FGCR1 : constant uint8 := 16#64#;   --  Foreground color register 1 (green)
   FGCR2 : constant uint8 := 16#65#;   --  Foreground color register 2 (blue)
--   PTNO  : constant uint8 := 16#66#;   --  Pattern set no for BTE
--   BGTR0 : constant uint8 := 16#67#;   --  Background color register for transparent 0 (red)
--   BGTR1 : constant uint8 := 16#68#;   --  Background color register for transparent 1 (green)
--   BGTR2 : constant uint8 := 16#69#;   --  Background color register for transparent 2 (blue)
   --
   TPCR0 : constant uint8 := 16#70#;   --  Touch panel control register 0
   TPCR0_ENABLE        : constant uint8 := 16#80#;
   TPCR0_DISABLE       : constant uint8 := 16#00#;
--   TPCR0_WAIT_512CLK   : constant uint8 := 16#00#;
--   TPCR0_WAIT_1024CLK  : constant uint8 := 16#10#;
--   TPCR0_WAIT_2048CLK  : constant uint8 := 16#20#;
   TPCR0_WAIT_4096CLK  : constant uint8 := 16#30#;
--   TPCR0_WAIT_8192CLK  : constant uint8 := 16#40#;
--   TPCR0_WAIT_16384CLK : constant uint8 := 16#50#;
--   TPCR0_WAIT_32768CLK : constant uint8 := 16#60#;
--   TPCR0_WAIT_65536CLK : constant uint8 := 16#70#;
   TPCR0_WAKEENABLE    : constant uint8 := 16#08#;
--   TPCR0_WAKEDISABLE   : constant uint8 := 16#00#;
--   TPCR0_ADCCLK_DIV1   : constant uint8 := 16#00#;
--   TPCR0_ADCCLK_DIV2   : constant uint8 := 16#01#;
   TPCR0_ADCCLK_DIV4   : constant uint8 := 16#02#;
--   TPCR0_ADCCLK_DIV8   : constant uint8 := 16#03#;
   TPCR0_ADCCLK_DIV16  : constant uint8 := 16#04#;
--   TPCR0_ADCCLK_DIV32  : constant uint8 := 16#05#;
--   TPCR0_ADCCLK_DIV64  : constant uint8 := 16#06#;
--   TPCR0_ADCCLK_DIV128 : constant uint8 := 16#07#;
   --
   TPCR1 : constant uint8 := 16#71#;   --  Touch panel control register 1
   TPCR1_AUTO       : constant uint8 := 16#00#;
--   TPCR1_MANUAL     : constant uint8 := 16#40#;
--   TPCR1_VREFINT    : constant uint8 := 16#00#;
--   TPCR1_VREFEXT    : constant uint8 := 16#20#;
   TPCR1_DEBOUNCE   : constant uint8 := 16#04#;
--   TPCR1_NODEBOUNCE : constant uint8 := 16#00#;
--   TPCR1_IDLE       : constant uint8 := 16#00#;
--   TPCR1_WAIT       : constant uint8 := 16#01#;
--   TPCR1_LATCHX     : constant uint8 := 16#02#;
--   TPCR1_LATCHY     : constant uint8 := 16#03#;
   --
   LTPR0 : constant uint8 := 16#52#;   --  Layer transparency register 0
   LTPR0_MODE_SCALE : uint8 := 16#40#;
   LTPR0_FLOAT_ENABLE : uint8 := 16#20#;
   --
   TPXH : constant uint8 := 16#72#;   --  Touch panel X high byte data register
   TPYH : constant uint8 := 16#73#;   --  Touch panel Y high byte data register
   --
   TPXYL : constant uint8 := 16#74#;   --  Touch panel X/Y low byte data register
--   TPXYL_TOUCHED : constant uint8 := 16#80#;
   TPXYL_Y_LSB   : constant uint8 := 16#0C#;
   TPXYL_X_LSB   : constant uint8 := 16#03#;
   --
   GCHP0 : constant uint8 := 16#80#;   --  Graphic cursor horizontal register 0
   GCHP1 : constant uint8 := 16#81#;   --  Graphic cursor horizontal register 1
   GCHV0 : constant uint8 := 16#82#;   --  Graphic cursor vertical register 0
   GCHV1 : constant uint8 := 16#83#;   --  Graphic cursor vertical register 1
   GCC0  : constant uint8 := 16#84#;   --  Graphics cursor color 0
   GCC1  : constant uint8 := 16#85#;   --  Graphics cursor color 1
   --
   PLLC1 : constant uint8 := 16#88#;   --  PLL control register 1
   PLLC1_PLLDIV1 : constant uint8 := 16#00#;
--   PLLC1_PLLDIV2 : constant uint8 := 16#80#;
   --
   PLLC2 : constant uint8 := 16#89#;   --  PLL control register 2
--   PLLC2_DIV1   : constant uint8 := 16#00#;
--   PLLC2_DIV2   : constant uint8 := 16#01#;
   PLLC2_DIV4   : constant uint8 := 16#02#;
--   PLLC2_DIV8   : constant uint8 := 16#03#;
--   PLLC2_DIV16  : constant uint8 := 16#04#;
--   PLLC2_DIV32  : constant uint8 := 16#05#;
--   PLLC2_DIV64  : constant uint8 := 16#06#;
--   PLLC2_DIV128 : constant uint8 := 16#07#;
   --
   --  PWM Control registers.  Note that PWM1 and PWM use the same values, so
   --  there is no point defining separate constants for each.  Only the
   --  addresses are different.
   P1CR  : constant uint8 := 16#8A#;
   P1DCR : constant uint8 := 16#8B#;
   P2CR  : constant uint8 := 16#8C#;
   P2DCR : constant uint8 := 16#8D#;
   --
   --  Constants for PWM CR
   PWMCR_ENABLE  : constant uint8 := 16#80#;
   PWMCR_DISABLE : constant uint8 := 16#00#;
--   PWMCR_CLKOUT  : constant uint8 := 16#10#;
--   PWMCR_PWMOUT  : constant uint8 := 16#00#;
   --
   --  Constants for PWM DCR
--   PWM_CLK_DIV1     : constant uint8 := 16#00#;
--   PWM_CLK_DIV2     : constant uint8 := 16#01#;
--   PWM_CLK_DIV4     : constant uint8 := 16#02#;
--   PWM_CLK_DIV8     : constant uint8 := 16#03#;
--   PWM_CLK_DIV16    : constant uint8 := 16#04#;
--   PWM_CLK_DIV32    : constant uint8 := 16#05#;
--   PWM_CLK_DIV64    : constant uint8 := 16#06#;
--   PWM_CLK_DIV128   : constant uint8 := 16#07#;
--   PWM_CLK_DIV256   : constant uint8 := 16#08#;
--   PWM_CLK_DIV512   : constant uint8 := 16#09#;
--   PWM_CLK_DIV1024  : constant uint8 := 16#0A#;
--   PWM_CLK_DIV2048  : constant uint8 := 16#0B#;
--   PWM_CLK_DIV4096  : constant uint8 := 16#0C#;
--   PWM_CLK_DIV8192  : constant uint8 := 16#0D#;
--   PWM_CLK_DIV16384 : constant uint8 := 16#0E#;
--   PWM_CLK_DIV32768 : constant uint8 := 16#0F#;
   --
   MCLR : constant uint8 := 16#8E#;   --  Memory clear register
   MCLR_START  : constant uint8 := 16#80#;
--   MCLR_STOP   : constant uint8 := 16#00#;
--   MCLR_READSTATUS : constant uint8 := 16#80#;
   MCLR_FULL   : constant uint8 := 16#00#;
--   MCLR_ACTIVE : constant uint8 := 16#40#;
   --
   DCR : constant uint8 := 16#90#;   --  Draw line/circle/square control register
   DCR_LINESQUTRI_START  : constant uint8 := 16#80#;
--   DCR_LINESQUTRI_STOP   : constant uint8 := 16#00#;
   DCR_LINESQUTRI_STATUS : constant uint8 := 16#80#;
   DCR_CIRCLE_START      : constant uint8 := 16#40#;
   DCR_CIRCLE_STATUS     : constant uint8 := 16#40#;
--   DCR_CIRCLE_STOP       : constant uint8 := 16#00#;
   DCR_FILL              : constant uint8 := 16#20#;
--   DCR_NOFILL            : constant uint8 := 16#00#;
   DCR_DRAWLINE          : constant uint8 := 16#00#;
   DCR_DRAWTRIANGLE      : constant uint8 := 16#01#;
   DCR_DRAWSQUARE        : constant uint8 := 16#10#;
   --
   DLHSR0 : constant uint8 := 16#91#;   --  Draw line/square horizontal start address register 0
   DLHSR1 : constant uint8 := 16#92#;   --  Draw line/square horizontal start address register 1
   DLVSR0 : constant uint8 := 16#93#;   --  Draw line/square vertical start address register 0
   DLVSR1 : constant uint8 := 16#94#;   --  Draw line/square vertical start address register 1
   DLHER0 : constant uint8 := 16#95#;   --  Draw line/square horizontal end address register 0
   DLHER1 : constant uint8 := 16#96#;   --  Draw line/square horizontal end address register 1
   DLVER0 : constant uint8 := 16#97#;   --  Draw line/square vertical end address register 0
   DLVER1 : constant uint8 := 16#98#;   --  Draw line/square vertical end address register 1
   DCHR0  : constant uint8 := 16#99#;   --  Draw circle center horizontal address register 0
   DCHR1  : constant uint8 := 16#9A#;   --  Draw circle center horizontal address register 1
   DCHV0  : constant uint8 := 16#9B#;   --  Draw circle center vertical address register 0
   DCHV1  : constant uint8 := 16#9C#;   --  Draw circle center vertical address register 1
   DCRR   : constant uint8 := 16#9D#;   --  Draw circle radius register
   --
   ELLIPSE : constant uint8 := 16#A0#;   --  Draw ellipse/ellipse curve/circle square control register
   ELLIPSE_STATUS : constant uint8 := 16#80#;
   ELLIPSE_START  : constant uint8 := 16#80#;
   ELLIPSE_FILL   : constant uint8 := 16#40#;
   ELLIPSE_SQR    : constant uint8 := 16#20#;
   ELLIPSE_CURVE  : constant uint8 := 16#10#;
   --
   ELL_A0  : constant uint8 := 16#A1#;   --  Draw ellipse/circle square long axis setting register 0
   ELL_A1  : constant uint8 := 16#A2#;   --  Draw ellipse/circle square long axis setting register 1
   ELL_B0  : constant uint8 := 16#A3#;   --  Draw ellipse/circle square short axis setting register 0
   ELL_B1  : constant uint8 := 16#A4#;   --  Draw ellipse/circle square short axis setting register 1
   DEHR0   : constant uint8 := 16#A5#;   --  Draw ellipse/circle square center horizontal address register 0
   DEHR1   : constant uint8 := 16#A6#;   --  Draw ellipse/circle square center horizontal address register 1
   DEVR0   : constant uint8 := 16#A7#;   --  Draw ellipse/circle square center vertical address register 0
   DEVR1   : constant uint8 := 16#A8#;   --  Draw ellipse/circle square center vertical address register 1
   DTPH0   : constant uint8 := 16#A9#;   --  Draw triangle point 2 horizontal address register 0
   DTPH1   : constant uint8 := 16#AA#;   --  Draw triangle point 2 horizontal address register 1
   DTPV0   : constant uint8 := 16#AB#;   --  Draw triangle point 2 vertical address register 0
   DTPV1   : constant uint8 := 16#AC#;   --  Draw triangle point 2 vertical address register 1
--   SSAR0   : constant uint8 := 16#B0#;   --  DMA Source starting address reg 0
--   SSAR1   : constant uint8 := 16#B1#;   --  DMA Source starting address reg 1
--   SSAR2   : constant uint8 := 16#B2#;   --  DMA Source starting address reg 2
--   DTNR0   : constant uint8 := 16#B4#;   --  DMA Block width register 0 / DMA transfer number register 0
--   BWR1    : constant uint8 := 16#B5#;   --  DMA Block width register 1
--   DTNR1   : constant uint8 := 16#B6#;   --  DMA Block height register 0 / DMA transfer number register 1
--   BHR1    : constant uint8 := 16#B7#;   --  DMA Block height register 1
--   DNTR2   : constant uint8 := 16#B8#;   --  DMA source picture width register 0 / DMA transfer number register 2
--   SPWR1   : constant uint8 := 16#B9#;   --  DMA Source picture register 1
--   DMACR   : constant uint8 := 16#BF#;   --  DMA configuration register
--   KSCR1   : constant uint8 := 16#C0#;   --  Key-Scan control register 1
--   KSCR2   : constant uint8 := 16#C1#;   --  Key-Scan control register 2
--   KSRD0   : constant uint8 := 16#C2#;   --  Key-Scan data register 0
--   KSRD1   : constant uint8 := 16#C3#;   --  Key-Scan data register 1
--   KSRD2   : constant uint8 := 16#C4#;   --  Key-Scan data register 2
   GPIOX   : constant uint8 := 16#C7#;   --  Extra general purpose I/O register
--   FWSAXA0 : constant uint8 := 16#D0#;   --  Floating window start address XA 0
--   FWSAXA1 : constant uint8 := 16#D1#;   --  Floating window start address XA 1
--   FWSAYA0 : constant uint8 := 16#D2#;   --  Floating window start address YA 0
--   FWSAYA1 : constant uint8 := 16#D3#;   --  Floating window start address YA 1
--   FWW0    : constant uint8 := 16#D4#;   --  Floating window width 0
--   FWW1    : constant uint8 := 16#D5#;   --  Floating window width 1
--   FWH0    : constant uint8 := 16#D6#;   --  Floating window height 0
--   FWH1    : constant uint8 := 16#D7#;   --  Floating window height 1
--   FWDXA0  : constant uint8 := 16#D8#;   --  Floating window display X address 0
--   FWDXA1  : constant uint8 := 16#D9#;   --  Floating window display X address 1
--   FWDYA0  : constant uint8 := 16#DA#;   --  Floating window display Y address 0
--   FWDYA1  : constant uint8 := 16#DB#;   --  Floating window display Y address 1
--   SACS_MODE : constant uint8 := 16#E0#;   --  Serial flash/ROM direct access mode
--   SACS_ADDR : constant uint8 := 16#E1#;   --  Serial flash/ROM direct access mode address
--   SACS_DATA : constant uint8 := 16#E2#;   --  Serial flash/ROM direct access data read
   --
   INTC1 : constant uint8 := 16#F0#;   --  Interrupt control register 1
--   INTC1_KEY : constant uint8 := 16#10#;
--   INTC1_DMA : constant uint8 := 16#08#;
   INTC1_TP  : constant uint8 := 16#04#;
--   INTC1_BTE : constant uint8 := 16#02#;
   --
   INTC2 : constant uint8 := 16#F1#;   --  Interrupt control register 2
--   INTC2_KEY : constant uint8 := 16#10#;
--   INTC2_DMA : constant uint8 := 16#08#;
   INTC2_TP  : constant uint8 := 16#04#;
--   INTC2_BTE : constant uint8 := 16#02#;
   ----------------------------------------------------------------------------
   --
   --  Internal functions
   --
   procedure writeCmd(self : RA8875_record; value : uint8);
   procedure writeData(self : RA8875_record; value : uint8);
   function readStatus(self : RA8875_record) return uint8;
   function readData(self : RA8875_record) return uint8;
   procedure writeReg(self : RA8875_record; reg : uint8; value : uint8);
   function readReg(self : RA8875_record; reg : uint8) return uint8;
   ----------------------------------------------------------------------------
   type RA8875_record is tagged
      record
         cs_gpio    : BBS.embed.GPIO.GPIO;
         reset_gpio : BBS.embed.GPIO.GPIO;
         lcd_screen : BBS.embed.SPI.SPI_ptr;
         size      : RA8875_sizes;
         width     : uint16;
         height    : uint16;
         cal_top   : uint16;
         cal_bot   : uint16;
         cal_left  : uint16;
         cal_right : uint16;
      end record;
   --
   -- Constants for pin outputs
   --
   gpio_high : constant bit := 0;
   gpio_low  : constant bit := 1;
   --
   -- Constants for RA8875 transaction types
   --
   DATAWRITE : constant uint8 := 16#00#;
   DATAREAD  : constant uint8 := 16#40#;
   CMDWRITE  : constant uint8 := 16#80#;
   CMDREAD   : constant uint8 := 16#C0#;

end;
