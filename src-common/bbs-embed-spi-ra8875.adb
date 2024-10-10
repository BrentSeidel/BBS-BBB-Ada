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
package body BBS.embed.SPI.RA8875 is
   --
   --  Routines for RA8875 control
   --
   --  Setup the RA8875 object.  Note that the RA8875 has a hardware reset line.
   --  This line is optional.  Setup functions are provided for configurations
   --  with and without hardware reset.
   --
   --  Setup without hardware reset
   --
   procedure setup(self : in out RA8875_record; CS : GPIO.GPIO; screen : SPI_ptr) is
   begin
      self.cs_gpio := CS;
      self.reset_gpio := null;
      self.lcd_screen := screen;
      self.cs_gpio.set(gpio_high);
   end;
   --
   --  Setup with hardware reset
   --
   procedure setup(self : in out RA8875_record; CS : GPIO.GPIO; RST : GPIO.GPIO; screen : SPI_ptr) is
   begin
      self.cs_gpio := CS;
      self.reset_gpio := null;
      self.lcd_screen := screen;
      self.cs_gpio.set(gpio_high);
      self.reset_gpio.set(gpio_high);
      delay 0.1;
      self.reset_gpio.set(gpio_low);
      delay 0.1;
      self.reset_gpio.set(gpio_high);
      delay 0.01;
   end;
   --
   --  Toggle the hardware reset line if it has been configured, otherwise do
   --  nothing.
   --
   procedure hwReset(self : in out RA8875_record) is
   begin
      if (self.reset_gpio /= null) then
         self.reset_gpio.set(gpio_low);
         delay 0.1;
         self.reset_gpio.set(gpio_high);
         delay 0.01;
      end if;
   end;
   --
   --  Send commands to perform a software reset.
   --
   procedure swReset(self : in out RA8875_record) is
      temp : uint8;
   begin
      self.writeCmd(PWRR);
      temp := self.readData;
      temp := temp or PWRR_SOFTRESET;
      self.writeData(temp);
      delay 0.1;
      self.writeCmd(PWRR);
      temp := self.readData;
      temp := temp and not PWRR_SOFTRESET;
      self.writeData(temp);
   end;
   --
   --  The following set of routines are the low level interface to read from and
   --  write to the RA8875.
   --
   procedure writeCmd(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(CMDWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(gpio_high);
   end;
   --
   procedure writeData(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(DATAWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(gpio_high);
   end;
   --
   function readStatus(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(CMDREAD);
      temp := self.lcd_screen.get;
      self.cs_gpio.set(gpio_high);
      return temp;
   end;
   --
   function readData(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(DATAREAD);
      temp := self.lcd_screen.get;
      self.cs_gpio.set(gpio_high);
      return temp;
   end;
   --
   procedure writeReg(self : RA8875_record; reg : uint8; value : uint8) is
   begin
      self.writeCmd(reg);
      self.writeData(value);
   end;
   --
   function readReg(self : RA8875_record; reg : uint8) return uint8 is
   begin
      self.writeCmd(reg);
      return self.readData;
   end;
   --
   --  Configure PWM unit 1.  The AdaFruit breakout board uses this to control the
   --  backlight
   --
   procedure PWM1config(self : RA8875_record; state : boolean; clock : uint8) is
   begin
      if (state) then
         self.writeReg(P1CR, PWMCR_ENABLE or (clock and 16#0F#));
      else
         self.writeReg(P1CR, PWMCR_DISABLE or (clock and 16#0F#));
      end if;
   end;
   --
   --  Configure PWM unit 2.
   procedure PWM2config(self : RA8875_record; state : boolean; clock : uint8) is
   begin
      if (state) then
         self.writeReg(P2CR, PWMCR_ENABLE or (clock and 16#0F#));
      else
         self.writeReg(P2CR, PWMCR_DISABLE or (clock and 16#0F#));
      end if;
   end;
   --
   --  Set PWM unit 1 output value.  On the AdaFruit breakout board, this sets the
   --  brightness of the backlight.
   --
   procedure PWM1out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(P1DCR, value);
   end;
   --
   --  Set the PWM unit 2 output value.
   procedure PWM2out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(P2DCR, value);
   end;
   --
   --  Configure the basic RA8875 settings.
   --
   procedure configure(self : in out RA8875_record; size : RA8875_sizes) is
      pixclk         : uint8;
      hsync_start    : uint8;
      hsync_pw       : uint8;
      hsync_finetune : uint8;
      hsync_nondisp  : uint8;
      vsync_pw       : uint8;
      vsync_nondisp  : uint16;
      vsync_start    : uint16;
   begin
      self.size := size;
      case size is
         when RA8875_480x272 =>
            self.writeReg(PLLC1, PLLC1_PLLDIV1 + 10);
            delay 0.001;
            self.writeReg(PLLC2, PLLC2_DIV4);
            delay 0.001;
            self.writeReg(SYSR, SYSR_16BPP or SYSR_MCU8);
            --
            pixclk         := PCSR_PDATL or PCSR_4CLK;
            hsync_nondisp  := 10;
            hsync_start    := 8;
            hsync_pw       := 48;
            hsync_finetune := 0;
            vsync_nondisp  := 3;
            vsync_start    := 8;
            vsync_pw       := 10;
            self.width     := 480;
            self.height    := 272;
         when RA8875_800x480 =>
            self.writeReg(PLLC1, PLLC1_PLLDIV1 + 10);
            delay 0.001;
            self.writeReg(PLLC2, PLLC2_DIV4);
            delay 0.001;
            self.writeReg(SYSR, SYSR_16BPP or SYSR_MCU8);
            --
            pixclk         := PCSR_PDATL or PCSR_2CLK;
            hsync_nondisp  := 26;
            hsync_start    := 32;
            hsync_pw       := 96;
            hsync_finetune := 0;
            vsync_nondisp  := 32;
            vsync_start    := 23;
            vsync_pw       := 2;
            self.width     := 800;
            self.height    := 480;
         when others =>
            Ada.Text_IO.Put_Line("RA8875 unknown LCD size.");
            return;
      end case;
      self.cal_top   := 0;
      self.cal_bot   := self.height;
      self.cal_left  := 0;
      self.cal_right := self.width;
      --
      self.writeReg(PCSR, pixclk);
      delay 0.001;
      --
      self.writeReg(HDWR, uint8((integer(self.width) / 8) - 1));
      self.writeReg(HNDFTR, HNDFTR_DE_HIGH + hsync_finetune);
      self.writeReg(HNDR, (hsync_nondisp - hsync_finetune - 2)/8);
      self.writeReg(HSTR, hsync_start/8 - 1);
      self.writeReg(HPWR, HPWR_LOW + (hsync_pw/8 - 1));
      --
      self.writeReg(VDHR0, lowByte(self.height - 1));   --  Set display height
      self.writeReg(VDHR1, highByte(self.height - 1));
      self.writeReg(VNDR0, lowByte(vsync_nondisp - 1));
      self.writeReg(VNDR1, highByte(vsync_nondisp));
      self.writeReg(VSTR0, lowByte(vsync_start - 1));
      self.writeReg(VSTR1, highByte(vsync_start));
      self.writeReg(VPWR, VPWR_LOW + vsync_pw - 1);
      --
      self.writeReg(HSAW0, 0);
      self.writeReg(HSAW1, 0);
      self.writeReg(HEAW0, lowByte(self.width - 1));
      self.writeReg(HEAW1, highByte(self.width - 1));
      --
      self.writeReg(VSAW0, 0);
      self.writeReg(VSAW1, 0);
      self.writeReg(VEAW0, lowByte(self.height - 1));
      self.writeReg(VEAW1, highByte(self.height - 1));
      --
      self.writeReg(MCLR, MCLR_START or MCLR_FULL);
      delay 0.5;
   end;
   --
   --  Set the display on or off
   --
   procedure setDisplay(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(PWRR, PWRR_NORMAL or PWRR_DISPON);
      else
         self.writeReg(PWRR, PWRR_NORMAL or PWRR_DISPOFF);
      end if;
   end;
   --
   --  Set the sleep mode of the display
   --
   procedure setSleep(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(PWRR, PWRR_DISPOFF or PWRR_SLEEP);
      else
         self.writeReg(PWRR, PWRR_DISPOFF);
      end if;
   end;
   --
   --  Set the state of the GPIOX pin.  This is used by the AdaFruit breakout board.
   procedure set_GPIOX(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(GPIOX, 1);
      else
         self.writeReg(GPIOX, 0);
      end if;
   end;
   --
   --  Set some of the display control parameters
   --
   procedure setDisplayCtrl(self : RA8875_record; layer : uint8; hdir : uint8;
                            vdir : uint8) is
   begin
      self.writeReg(DPCR, layer or hdir or vdir);
   end;
   --
   procedure setWriteCtrl0(self : RA8875_record; mode : MWCR0_MODE; cursorVisible : boolean;
                           cursorBlink : boolean; writeDir : MWCR0_CURDIR; WriteCursorIncr : boolean;
                           ReadCursorIncr : boolean) is
      temp : uint8 := 0;
   begin
      if (mode = text) then
         temp := temp + MWCR0_TXTMODE;
      end if;
      if (cursorVisible) then
         temp := temp + MWCR0_CURVIS;
      end if;
      if (cursorBlink) then
         temp := temp + MWCR0_CURBLINK;
      end if;
      temp := temp + (MWCR0_CURDIR'pos(writeDir) * 16#04#);
      if (WriteCursorIncr) then
         temp := temp + MWCR0_WRITE_NOINCR;
      end if;
      if (ReadCursorIncr) then
         temp := temp + MWCR0_READ_NOINCR;
      end if;
      self.writeReg(MWCR0, temp);
   end;
   --
   --  Memory write control register 1
   procedure setWriteCtrl1(self : RA8875_record; cursorEnable : MWCR1_GCURS_ENABLE;
                           GCursorSelect : MWCR1_GCURS_SET; writeDest : MWCR1_WRITE_DEST;
                           layer : RA8875_LAYER) is
      temp : uint8 := 0;
   begin
      if (cursorEnable = enable) then
         temp := temp + MWCR1_GCURS_EN;
      end if;
      temp := temp + uint8(GCursorSelect)*MWCR1_CUR_SEL_SCALE;
      temp := temp + MWCR1_WRITE_DEST'pos(writeDest)*MWCR1_WRITE_DEST_SCALE;
      temp := temp + RA8875_LAYER'pos(layer);
      self.writeReg(MWCR1, temp);
   end;
   --
   ----------------------------------------------------------------------------
   --  Text items
   --
   procedure textMode(self : RA8875_record) is
      temp : uint8;
   begin
      --
      --  Set mode
      self.writeCmd(MWCR0);
      temp := self.readData;
      temp := temp or MWCR0_TXTMODE;
      self.writeData(temp);
      --
      --  Select internal font
      self.writeCmd(FNCR0);
      temp := self.readData;
      temp := temp and not (FNCR0_CGRAM or FNCR0_EXTCR);
      self.writeData(temp);
   end;
   --
   procedure textColor(self : RA8875_record; bg : R5G6B5_color; fg : R5G6B5_color) is
      temp : uint8;
   begin
      --  Set Fore Color
      self.writeReg(FGCR0, fg.R);
      self.writeReg(FGCR1, fg.G);
      self.writeReg(FGCR2, fg.B);
      --
      --  Set Background Color
      self.writeReg(BGCR0, bg.R);
      self.writeReg(BGCR1, bg.G);
      self.writeReg(BGCR2, bg.B);
      --
      --  Clear transparency flag
      self.writeCmd(FNCR1);
      temp := self.readData;
      temp := temp and not 64;
      self.writeData(temp);
   end;
   --
   procedure textSetCodePage(self : RA8875_record; page : FNCR0_Code_Page) is
      temp : uint8;
   begin
      self.writeCmd(FNCR0);
      temp := self.readData;
      temp := (temp and 16#FC#) or uint8(FNCR0_Code_Page'pos(page));
      self.writeData(temp);
   end;
   --
   procedure textSetAttribute(self : RA8875_record; align : boolean; transparent : boolean;
                              rotate : boolean; h_size : uint8; v_size : uint8) is
      temp : uint8 := 0;
   begin
      temp := temp or (if (align) then FNCR1_ALIGN else 0);
      temp := temp or (if (transparent) then FNCR1_TRANS else 0);
      temp := temp or (if (rotate) then FNCR1_ROT else 0);
      temp := temp or ((h_size and 16#03#) * 16#04#);
      temp := temp or (v_size and 16#03#);
      self.writeReg(FNCR1, temp);
   end;
   --
   procedure textSetLineHeight(self : RA8875_record; size : uint8) is
   begin
      self.writeReg(FLDR, size);
   end;
   --
   procedure textSetFontWidth(self : RA8875_record; size : uint8) is
      temp : uint8;
   begin
      self.writeCmd(FWTSR);
      temp := self.readData;  --  BUG? Constant on next line should probably be 16#C0# rather than 16#FC#.
      temp := (temp and 16#FC#) or (size and 16#3F#);
      self.writeData(temp);
   end;
   --
   procedure textWrite(self : RA8875_record; str : string) is
   begin
      self.writeCmd(MRWC);
      for temp of str loop
         self.writeData(uint8(character'pos(temp)));
         delay 0.001;
      end loop;
   end;
   ---------------------------------------------------------------------------
   --  Graphics items
   --
   --  This section contains routines to access the graphics primitves offered
   --  by the RA8875.
   --
   procedure graphicsMode(self : RA8875_record) is
      temp : uint8;
   begin
      self.writeCmd(MWCR0);
      temp := self.readData;
      temp := temp and not MWCR0_TXTMODE;
      self.writeData(temp);
   end;
   --
   --  Set color for drawing
   --
   procedure drawColor(self : RA8875_record; color : R5G6B5_color) is
      begin
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
   end;
   --
   --  Draw a rectangle
   --
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeCmd(DCR);
      if (fill) then
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWSQUARE
                      or DCR_FILL);
      else
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWSQUARE);
      end if;
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      self.writeCmd(DCR);
      if (fill) then
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWSQUARE
                      or DCR_FILL);
      else
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWSQUARE);
      end if;
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   --  Draw a rectangle with rounded corners
   --
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                         y2 : uint16; rad : uint16; fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeReg(ELL_A0, lowByte(rad));
      self.writeReg(ELL_A1, highByte(rad));
      --
      self.writeReg(ELL_B0, lowByte(rad));
      self.writeReg(ELL_B1, highByte(rad));
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_SQR
                      or ELLIPSE_FILL);
      else
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_SQR);
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                         y2 : uint16; rad : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeReg(ELL_A0, lowByte(rad));
      self.writeReg(ELL_A1, highByte(rad));
      --
      self.writeReg(ELL_B0, lowByte(rad));
      self.writeReg(ELL_B1, highByte(rad));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_SQR
                      or ELLIPSE_FILL);
      else
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_SQR);
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   --  Draw a line
   --
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeCmd(DCR);
      self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWLINE);
      --
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DLHER0, lowByte(x2));
      self.writeReg(DLHER1, highByte(x2));
      self.writeReg(DLVER0, lowByte(y2));
      self.writeReg(DLVER1, highByte(y2));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      self.writeCmd(DCR);
      self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWLINE);
      --
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   --  Draw a circle
   --
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        fill : boolean) is
   begin
      self.writeReg(DCHR0, lowByte(x));
      self.writeReg(DCHR1, highByte(x));
      self.writeReg(DCHV0, lowByte(y));
      self.writeReg(DCHV1, highByte(y));
      --
      self.writeReg(DCRR, lowByte(rad));
      --
      self.writeCmd(DCR);
      if (fill) then
         self.writeReg(DCR, DCR_CIRCLE_START or DCR_FILL);
      else
         self.writeReg(DCR, DCR_CIRCLE_START);
      end if;
      self.waitPoll(DCR, DCR_CIRCLE_STATUS);
   end;
   --
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DCHR0, lowByte(x));
      self.writeReg(DCHR1, highByte(x));
      self.writeReg(DCHV0, lowByte(y));
      self.writeReg(DCHV1, highByte(y));
      --
      self.writeReg(DCRR, lowByte(rad));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      self.writeCmd(DCR);
      if (fill) then
         self.writeReg(DCR, DCR_CIRCLE_START or DCR_FILL);
      else
         self.writeReg(DCR, DCR_CIRCLE_START);
      end if;
      self.waitPoll(DCR, DCR_CIRCLE_STATUS);
   end;
   --
   --  Draw a triangle
   --
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      --
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DTPH0, lowByte(x2));
      self.writeReg(DTPH1, highByte(x2));
      --
      self.writeReg(DTPV0, lowByte(y2));
      self.writeReg(DTPV1, highByte(y2));
      --
      self.writeReg(DLHER0, lowByte(x3));
      self.writeReg(DLHER1, highByte(x3));
      --
      self.writeReg(DLVER0, lowByte(y3));
      self.writeReg(DLVER1, highByte(y3));
      --
      if (fill) then
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWTRIANGLE
                      or DCR_FILL);
      else
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWTRIANGLE);
      end if;
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DLHSR0, lowByte(x1));
      self.writeReg(DLHSR1, highByte(x1));
      --
      self.writeReg(DLVSR0, lowByte(y1));
      self.writeReg(DLVSR1, highByte(y1));
      --
      self.writeReg(DTPH0, lowByte(x2));
      self.writeReg(DTPH1, highByte(x2));
      --
      self.writeReg(DTPV0, lowByte(y2));
      self.writeReg(DTPV1, highByte(y2));
      --
      self.writeReg(DLHER0, lowByte(x3));
      self.writeReg(DLHER1, highByte(x3));
      --
      self.writeReg(DLVER0, lowByte(y3));
      self.writeReg(DLVER1, highByte(y3));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWTRIANGLE
                      or DCR_FILL);
      else
         self.writeReg(DCR, DCR_LINESQUTRI_START or DCR_DRAWTRIANGLE);
      end if;
      self.waitPoll(DCR, DCR_LINESQUTRI_STATUS);
   end;
   --
   --  Draw an ellipse
   --
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; fill : boolean) is
   begin
      self.writeReg(DEHR0, lowByte(x));
      self.writeReg(DEHR1, highByte(x));
      self.writeReg(DEVR0, lowByte(y));
      self.writeReg(DEVR1, highByte(y));
      --
      self.writeReg(ELL_A0, lowByte(hRad));
      self.writeReg(ELL_A1, highByte(hRad));
      --
      self.writeReg(ELL_B0, lowByte(vRad));
      self.writeReg(ELL_B1, highByte(vRad));
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_FILL);
      else
         self.writeReg(ELLIPSE, ELLIPSE_START);
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DEHR0, lowByte(x));
      self.writeReg(DEHR1, highByte(x));
      self.writeReg(DEVR0, lowByte(y));
      self.writeReg(DEVR1, highByte(y));
      --
      self.writeReg(ELL_A0, lowByte(hRad));
      self.writeReg(ELL_A1, highByte(hRad));
      --
      self.writeReg(ELL_B0, lowByte(vRad));
      self.writeReg(ELL_B1, highByte(vRad));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_FILL);
      else
         self.writeReg(ELLIPSE, ELLIPSE_START);
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   --  Draw one of four segments of an ellipse
   --
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; seg : ELLIPSE_PART; fill : boolean) is
   begin
      self.writeReg(DEHR0, lowByte(x));
      self.writeReg(DEHR1, highByte(x));
      self.writeReg(DEVR0, lowByte(y));
      self.writeReg(DEVR1, highByte(y));
      --
      self.writeReg(ELL_A0, lowByte(hRad));
      self.writeReg(ELL_A1, highByte(hRad));
      --
      self.writeReg(ELL_B0, lowByte(vRad));
      self.writeReg(ELL_B1, highByte(vRad));
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_CURVE or
                         ELLIPSE_FILL or uint8(ELLIPSE_PART'pos(seg)));
      else
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_CURVE or
                      uint8(ELLIPSE_PART'pos(seg)));
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; seg : ELLIPSE_PART; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(DEHR0, lowByte(x));
      self.writeReg(DEHR1, highByte(x));
      self.writeReg(DEVR0, lowByte(y));
      self.writeReg(DEVR1, highByte(y));
      --
      self.writeReg(ELL_A0, lowByte(hRad));
      self.writeReg(ELL_A1, highByte(hRad));
      --
      self.writeReg(ELL_B0, lowByte(vRad));
      self.writeReg(ELL_B1, highByte(vRad));
      --
      self.writeReg(FGCR0, color.R);
      self.writeReg(FGCR1, color.G);
      self.writeReg(FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_CURVE or
                         ELLIPSE_FILL or uint8(ELLIPSE_PART'pos(seg)));
      else
         self.writeReg(ELLIPSE, ELLIPSE_START or ELLIPSE_CURVE or
                      uint8(ELLIPSE_PART'pos(seg)));
      end if;
      self.waitPoll(ELLIPSE, ELLIPSE_STATUS);
   end;
   --
   procedure waitPoll(self : RA8875_record; reg : uint8; flag : uint8) is
      temp : uint8;
   begin
      loop
         temp := self.readReg(reg);
         exit when ((temp and flag) = 0);
      end loop;
   end;
   ----------------------------------------------------------------------------
   --  Touch items
   --
   procedure enableTouch(self : RA8875_record; state : boolean) is
      adcClock : uint8;
      temp : uint8;
   begin
      case self.width is
         when 480 =>
            adcClock := TPCR0_ADCCLK_DIV4;
         when 800 =>
            adcClock := TPCR0_ADCCLK_DIV16;
         when others =>
            adcClock := TPCR0_ADCCLK_DIV4;
      end case;
      if (state) then
         self.writeReg(TPCR0, TPCR0_ENABLE or TPCR0_WAIT_4096CLK or
                    TPCR0_WAKEENABLE or adcClock);
         self.writeReg(TPCR1, TPCR1_AUTO or TPCR1_DEBOUNCE);
         self.writeCmd(INTC1);
         temp := self.readData;
         temp := temp and INTC1_TP;
         self.writeData(temp);
      else
         self.writeReg(TPCR0, TPCR0_DISABLE);
         self.writeCmd(INTC1);
         temp := self.readData;
         temp := temp and not INTC1_TP;
         self.writeData(temp);
      end if;
   end;
   --
   function checkTouched(self : RA8875_record) return boolean is
      temp : uint8 := self.readReg(INTC2);
   begin
      self.writeReg(INTC2, INTC2_TP);
      if ((temp and INTC2_TP) = 0) then
         return false;
      else
         return true;
      end if;
   end;
   --
   procedure readTouchRaw(self : RA8875_record; x : out uint16; y : out uint16) is
      x_msb : uint8 := self.readReg(TPXH);
      y_msb : uint8 := self.readReg(TPYH);
      xy_lsb : uint8 := self.readReg(TPXYL);
   begin
      x := uint16(x_msb)*4 + uint16(xy_lsb and TPXYL_X_LSB);
      y := uint16(y_msb)*4 + uint16(xy_lsb and TPXYL_Y_LSB)/4;
   end;
   --
   procedure readTouchCal(self : RA8875_record; x : out uint16; y : out uint16) is
      x_msb : uint8 := self.readReg(TPXH);
      y_msb : uint8 := self.readReg(TPYH);
      xy_lsb : uint8 := self.readReg(TPXYL);
      temp_x : float;
      temp_y : float;
   begin
      temp_x := float(x_msb)*4.0 + float(xy_lsb and TPXYL_X_LSB);
      temp_y := float(y_msb)*4.0 + float((xy_lsb and TPXYL_Y_LSB)/4);
      temp_x := (temp_x - float(self.cal_left))*float(self.width)/float(self.cal_right - self.cal_left);
      temp_y := (temp_y - float(self.cal_top))*float(self.height)/float(self.cal_bot - self.cal_top);
      if (temp_x < 0.0) then
         x := 0;
      elsif (temp_x > float(self.width)) then
         x := self.width;
      else
         x := uint16(temp_x);
      end if;
      if (temp_y < 0.0) then
         y := 0;
      elsif (temp_y > float(self.height)) then
         y := self.height;
      else
         y := uint16(temp_y);
      end if;
   end;
   --
   procedure setTouchCalibration(self : in out RA8875_record; top : uint16;
                                 bottom : uint16; left : uint16; right : uint16) is
   begin
      self.cal_top := top;
      self.cal_bot := bottom;
      self.cal_left := left;
      self.cal_right := right;
   end;
   --
   procedure getTouchCalibration(self : RA8875_record; top : out uint16;
                                 bottom : out uint16; left : out uint16; right : out uint16) is
   begin
      top := self.cal_top;
      bottom := self.cal_bot;
      left := self.cal_left;
      right := self.cal_right;
   end;
   --
   procedure touchCalibrate(self : in out RA8875_record) is
      temp_x : uint16;
      temp_y : uint16;
      found_top : uint16 := 65535;
      found_bot : uint16 := 0;
      found_left : uint16 := 65535;
      found_right : uint16 := 0;
      count : integer := 0;
      samples : constant integer := 16;
      off_count : constant integer := 100;
   begin
      self.cal_top := 0;
      self.cal_bot := self.height;
      self.cal_left := 0;
      self.cal_right := self.width;
      self.graphicsMode;
      self.fillScreen(R5G6B5_BLACK);
      self.enableTouch(true);
      -- Find top edge
      self.textMode;
      self.screenActive;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(R5G6B5_BLACK, R5G6B5_WHITE);
      self.textSetCodePage(FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.setTextCursorPos(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Top Edge");
      self.graphicsMode;
      self.drawRect(0, 0, self.width - 1, 10, R5G6B5_WHITE, true);
      loop
         if (self.checkTouched) then
            count := count + 1;
            self.readTouchRaw(temp_x, temp_y);
            if (temp_y < found_top) then
               found_top := temp_y;
               end if;
            Ada.Text_IO.Put_Line("Found top touch " & integer'Image(integer(temp_x)) &
                                   integer'Image(integer(temp_y)));
         else
            count := 0;
         end if;
         exit when count >= samples;
      end loop;
      Ada.Text_IO.Put_Line("Top calibration value is " & integer'Image(integer(found_top)));
      count := 0;
      loop
         if (not self.checkTouched) then
            count := count + 1;
         else
            count := 0;
         end if;
         exit when count > off_count;
      end loop;
      delay 0.5;
      -- Find bottom edge
      self.graphicsMode;
      self.fillScreen(R5G6B5_BLACK);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(R5G6B5_BLACK, R5G6B5_WHITE);
      self.textSetCodePage(FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.setTextCursorPos(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Bottom Edge");
      self.graphicsMode;
      self.drawRect(0, self.height - 10, self.width - 1, self.height - 1, R5G6B5_WHITE, true);
      count := 0;
      loop
         if (self.checkTouched) then
            count := count + 1;
            self.readTouchRaw(temp_x, temp_y);
            if (temp_y > found_bot) then
               found_bot := temp_y;
            end if;
            Ada.Text_IO.Put_Line("Found bottom touch " & integer'Image(integer(temp_x)) &
                                   integer'Image(integer(temp_y)));
         else
            count := 0;
         end if;
         exit when count >= samples;
      end loop;
      Ada.Text_IO.Put_Line("Bottom calibration value is " & integer'Image(integer(found_bot)));
      count := 0;
      loop
         if (not self.checkTouched) then
            count := count + 1;
         else
            count := 0;
         end if;
         exit when count > off_count;
      end loop;
      delay 0.5;
      -- Find left edge
      self.graphicsMode;
      self.fillScreen(R5G6B5_BLACK);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(R5G6B5_BLACK, R5G6B5_WHITE);
      self.textSetCodePage(FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.setTextCursorPos(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Left Edge");
      self.graphicsMode;
      self.drawRect(0, 0, 10, self.height - 1, R5G6B5_WHITE, true);
      count := 0;
      loop
         if (self.checkTouched) then
            count := count + 1;
            self.readTouchRaw(temp_x, temp_y);
            if (temp_x < found_left) then
               found_left := temp_x;
            end if;
            Ada.Text_IO.Put_Line("Found left value " & integer'Image(integer(temp_x)));
         else
            count := 0;
         end if;
         exit when count >= samples;
      end loop;
      Ada.Text_IO.Put_Line("Left calibration value is " & integer'Image(integer(found_left)));
      count := 0;
      loop
         if (not self.checkTouched) then
            count := count + 1;
         else
            count := 0;
         end if;
         exit when count > off_count;
      end loop;
      delay 0.5;
      -- Find right edge
      self.graphicsMode;
      self.fillScreen(R5G6B5_BLACK);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(R5G6B5_BLACK, R5G6B5_WHITE);
      self.textSetCodePage(FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.setTextCursorPos(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Right Edge");
      self.graphicsMode;
      self.drawRect(self.width - 10, 0, self.width - 1, self.height - 1, R5G6B5_WHITE, true);
      count := 0;
      loop
         if (self.checkTouched) then
            count := count + 1;
            self.readTouchRaw(temp_x, temp_y);
            if (temp_x > found_right) then
               found_right := temp_x;
            end if;
            Ada.Text_IO.Put_Line("Found right value " & integer'Image(integer(temp_x)));
         else
            count := 0;
         end if;
         exit when count >= samples;
      end loop;
      Ada.Text_IO.Put_Line("Right calibration value is " & integer'Image(integer(found_Right)));
      count := 0;
      loop
         if (not self.checkTouched) then
            count := count + 1;
         else
            count := 0;
         end if;
         exit when count > off_count;
      end loop;
      delay 0.5;
      Ada.Text_IO.Put_Line("Top calibration : " & integer'Image(integer(found_top)));
      Ada.Text_IO.Put_Line("Bottom calibration : " & integer'Image(integer(found_bot)));
      Ada.Text_IO.Put_Line("Left calibration : " & integer'Image(integer(found_left)));
      Ada.Text_IO.Put_Line("Right calibration : " & integer'Image(integer(found_right)));
      self.cal_top := found_top;
      self.cal_bot := found_bot;
      self.cal_left := found_left;
      self.cal_right := found_right;
   end;
   ----------------------------------------------------------------------------
   --  Region and layer items
   --
   --  Define a scroll area and move it
   --
   procedure scroll(self : RA8875_record; hStart : uint16; vStart : uint16;
                    hEnd : uint16; vEnd : uint16; hOffset : uint16; vOffset : uint16) is
   begin
      self.writeReg(HSSW0, lowByte(hStart));
      self.writeReg(HSSW1, highByte(hStart));
      self.writeReg(VSSW0, lowByte(vStart));
      self.writeReg(VSSW1, highByte(vStart));
      --
      self.writeReg(HEAW0, lowByte(hEnd));
      self.writeReg(HEAW1, highByte(hEnd));
      self.writeReg(VEAW0, lowByte(vEnd));
      self.writeReg(VEAW1, highByte(vEnd));
      --
      if (hOffset /= 0) then
         self.writeReg(HOFS0, lowByte(hOffset));
         self.writeReg(HOFS1, highByte(hOffset));
      end if;
      if (vOffset /= 0) then
         self.writeReg(VOFS0, lowByte(vOffset));
         self.writeReg(VOFS1, highByte(vOffset));
      end if;
   end;
   --
   --  Set the bounds for the active window.
   --
   procedure setActiveWindow(self : RA8875_record; top : uint16; bottom : uint16;
                             left : uint16; right : uint16) is
   begin
      self.writeReg(HSAW0, lowByte(left));
      self.writeReg(HSAW1, highByte(left));
      --
      self.writeReg(VSAW0, lowByte(top));
      self.writeReg(VSAW1, highByte(top));
      --
      self.writeReg(HEAW0, lowByte(right));
      self.writeReg(HEAW1, highByte(right));
      --
      self.writeReg(VEAW0, lowByte(bottom));
      self.writeReg(VEAW1, highByte(bottom));
   end;
   --
   --  Set the active area to be the whole screen
   --
   procedure screenActive(self : RA8875_record) is
   begin
      self.setActiveWindow(0, self.height - 1, 0, self.width - 1);
   end;
   --
   procedure setLayers(self : RA8875_record; layer : RA8875_LAYER) is
      temp1 : uint8 := self.readReg(SYSR) and 16#03#;
      temp2 : uint8 := self.readReg(DPCR) and 16#7F#;
   begin
      if (layer = LAYER1) then
         temp1 := temp1 and SYSR_16BPP;
         temp2 := temp2 and DPCR_1LAYER;
      else
         temp1 := temp1 and SYSR_8BPP; -- Eventually this can be modified by size
         temp2 := temp2 and DPCR_2LAYER;
      end if;
      self.writeReg(SYSR, temp1);
      self.writeReg(DPCR, temp2);
   end;
   --
   procedure selectLayer(self : RA8875_record; layer : RA8875_LAYER) is
      temp : uint8 := self.readReg(MWCR1) and 16#F0#;
   begin
      temp := temp or RA8875_LAYER'pos(layer);
      self.writeReg(MWCR1, temp);
   end;
   --
   procedure setLayerSetting0(self : RA8875_record; scroll : LTPR0_SCROLL_MODE;
                              float : boolean; display : LTPR0_DISP_MODE) is
      temp : uint8 := LTPR0_SCROLL_MODE'pos(scroll)*LTPR0_MODE_SCALE;
   begin
      if (float) then
         temp := temp + LTPR0_FLOAT_ENABLE;
      end if;
      temp := temp + LTPR0_DISP_MODE'pos(display);
      self.writeReg(LTPR0, temp);
   end;
   --
   ----------------------------------------------------------------------------
   --  Cursor Methods
   --
   procedure setTextCursorPos(self : RA8875_record; x : uint16; y : uint16) is
   begin
      self.writeReg(F_CURXL, lowByte(x));
      self.writeReg(F_CURXH, highByte(x));
      self.writeReg(F_CURYL, lowByte(y));
      self.writeReg(F_CURYH, highByte(y));
   end;
   --
   procedure setGraphCursorColors(self : RA8875_record; color0 : R3G3B2_color;
                                  color1 : R3G3B2_color) is
   begin
      self.writeReg(GCC0, R3G3B2_to_uint8(color0));
      self.writeReg(GCC1, R3G3B2_to_uint8(color1));
   end;
   --
   procedure setGraphCursorPos(self : RA8875_record; x : uint16; y : uint16) is
   begin
      self.writeReg(GCHP0, lowByte(x));
      self.writeReg(GCHP1, highByte(x));
      self.writeReg(GCHV0, lowByte(y));
      self.writeReg(GCHV1, highByte(y));
   end;
   --
   procedure setGraphCursor(self : RA8875_record; curs : MWCR1_GCURS_SET;
                            data : RA8875_GCursor) is
      buffer : RA8875_GCursorBuffer := GCursor_to_buffer(data);
      temp   : uint8 := 0;
      Lmwcr0 : uint8 := self.readReg(MWCR0);
      Lmwcr1 : uint8 := self.readReg(MWCR1);
   begin
      self.setWriteCtrl0(graphic, true, false, LRTD, true, true);
      self.setWriteCtrl1(enable, curs, GCURS, layer1);
      self.writeCmd(MRWC);
      for t of buffer loop
         temp := (t and 16#F0#)/16 or (t and 16#0F#)*16;
         temp := (temp and 16#CC#)/4 or (temp and 16#33#)*4;
         self.writeData(temp);
         delay 0.001;
      end loop;
      self.writeReg(MWCR0, Lmwcr0);
      self.writeReg(MWCR1, Lmwcr1);
   end;
   --
   procedure selectGraphCursor(self : RA8875_record; curs : MWCR1_GCURS_SET;
                               enable : MWCR1_GCURS_ENABLE) is
      temp : uint8 := self.readReg(MWCR1);
   begin
      temp := temp and 16#0F#;
      if (enable = enable) then
         temp := temp + MWCR1_GCURS_EN;
      end if;
      temp := temp + uint8(curs)*MWCR1_CUR_SEL_SCALE;
      self.writeReg(MWCR1, temp);
   end;
   --
   ----------------------------------------------------------------------------
   --  Miscellaneous Methods
   --
   --  Fill the screen with a solid color.
   --
   procedure fillScreen(self : RA8875_record; color : R5G6B5_color) is
   begin
      self.drawRect(0, 0, self.width - 1, self.height - 1, color, true);
   end;
   --
end;
