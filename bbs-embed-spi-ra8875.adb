package body BBS.embed.SPI.RA8875 is
   --
   -- Create a new object
   --
   function RA8875_new return RA8875_ptr is
   begin
      return new RA8875_record;
   end;
   -------------------------------------------------------------------------------
   -- Routines for RA8875 control
   --
   procedure setup(self : in out RA8875_record; CS : BBS.embed.GPIO.GPIO; screen : SPI_ptr) is
   begin
      self.cs_gpio := CS;
      self.lcd_screen := screen;
      self.cs_gpio.set(cs_high);
      --
      -- Add code to toggle rst_gpio
      --
   end;
   --
   procedure writeCmd(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(cs_low);
      self.lcd_screen.set(RA8875_CMDWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(cs_high);
   end;
   --
   procedure writeData(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(cs_low);
      self.lcd_screen.set(RA8875_DATAWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(cs_high);
   end;
   --
   function readStatus(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(cs_low);
      self.lcd_screen.set(RA8875_CMDREAD);
      temp := self.lcd_screen.get;
      self.cs_gpio.set(cs_high);
      return temp;
   end;
   --
   function readData(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(cs_low);
      self.lcd_screen.set(RA8875_DATAREAD);
      temp := self.lcd_screen.get;
      self.cs_gpio.set(cs_high);
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
   procedure PWM1config(self : RA8875_record; state : boolean; clock : uint8) is
   begin
      if (state) then
         self.writeReg(RA8875_P1CR, RA8875_PWMCR_ENABLE or (clock and 16#0F#));
      else
         self.writeReg(RA8875_P1CR, RA8875_PWMCR_DISABLE or (clock and 16#0F#));
      end if;
   end;
   --
   procedure PWM2config(self : RA8875_record; state : boolean; clock : uint8) is
   begin
      if (state) then
         self.writeReg(RA8875_P2CR, RA8875_PWMCR_ENABLE or (clock and 16#0F#));
      else
         self.writeReg(RA8875_P2CR, RA8875_PWMCR_DISABLE or (clock and 16#0F#));
      end if;
   end;
   --
   procedure PWM1out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(RA8875_P1DCR, value);
   end;
   --
   procedure PWM2out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(RA8875_P2DCR, value);
   end;
   --
   procedure spi_configure(self : RA8875_record) is
      pixclk : uint8;
      hsync_start : uint8;
      hsync_pw : uint8;
      hsync_finetune : uint8;
      hsync_nondisp : uint8;
      vsync_pw : uint8;
      vsync_nondisp : uint16;
      vsync_start : uint16;
   begin
      Ada.Text_IO.Put_Line("Testing for presence of RA8875");
      Ada.Text_IO.Put_Line("Value expected 117, actual " & integer'Image(integer(self.readReg(0))));
      Ada.Text_IO.Put_Line("Configuring RA8875 PLL");
      --  if (_size == RA8875_480x272) {
      --    writeReg(RA8875_PLLC1, RA8875_PLLC1_PLLDIV1 + 10);
      --    delay(1);
      --    writeReg(RA8875_PLLC2, RA8875_PLLC2_DIV4);
      --    delay(1);
      --  }
      --  if (_size == RA8875_800x480) {
      --    writeReg(RA8875_PLLC1, RA8875_PLLC1_PLLDIV1 + 10);
      --    delay(1);
      --    writeReg(RA8875_PLLC2, RA8875_PLLC2_DIV4);
      --    delay(1);
      --  }
      self.writeReg(RA8875_PLLC1, RA8875_PLLC1_PLLDIV1 + 10);
      delay 0.001;
      self.writeReg(RA8875_PLLC2, RA8875_PLLC2_DIV4);
      delay 0.001;
      self.writeReg(RA8875_SYSR, RA8875_SYSR_16BPP or RA8875_SYSR_MCU8);
--  if (_size == RA8875_480x272)
--  {
--    pixclk          = RA8875_PCSR_PDATL | RA8875_PCSR_4CLK;
--    hsync_nondisp   = 10;
--    hsync_start     = 8;
--    hsync_pw        = 48;
--    hsync_finetune  = 0;
--    vsync_nondisp   = 3;
--    vsync_start     = 8;
--    vsync_pw        = 10;
--  }
--  else if (_size == RA8875_800x480)
--  {
--    pixclk          = RA8875_PCSR_PDATL | RA8875_PCSR_2CLK;
--    hsync_nondisp   = 26;
--    hsync_start     = 32;
--    hsync_pw        = 96;
--    hsync_finetune  = 0;
--    vsync_nondisp   = 32;
--    vsync_start     = 23;
--    vsync_pw        = 2;
--  }
      pixclk          := RA8875_PCSR_PDATL or RA8875_PCSR_2CLK;
      hsync_nondisp   := 26;
      hsync_start     := 32;
      hsync_pw        := 96;
      hsync_finetune  := 0;
      vsync_nondisp   := 32;
      vsync_start     := 23;
      vsync_pw        := 2;
      --
      self.writeReg(RA8875_PCSR, pixclk);
      delay 0.001;

      Ada.Text_IO.Put_Line("Configuring RA8875 horizontal settings");
      self.writeReg(RA8875_HDWR, uint8((integer(800) / 8) - 1));
      self.writeReg(RA8875_HNDFTR, RA8875_HNDFTR_DE_HIGH + hsync_finetune);
      self.writeReg(RA8875_HNDR, (hsync_nondisp - hsync_finetune - 2)/8);
      self.writeReg(RA8875_HSTR, hsync_start/8 - 1);
      self.writeReg(RA8875_HPWR, RA8875_HPWR_LOW + (hsync_pw/8 - 1));
      --
      Ada.Text_IO.Put_Line("Configuring RA8875 vertical settings");
      self.writeReg(RA8875_VDHR0, lowByte(480 - 1));
      self.writeReg(RA8875_VDHR1, highByte(480 - 1));
      self.writeReg(RA8875_VNDR0, lowByte(vsync_nondisp - 1));
      self.writeReg(RA8875_VNDR1, highByte(vsync_nondisp));
      self.writeReg(RA8875_VSTR0, lowByte(vsync_start - 1));
      self.writeReg(RA8875_VSTR1, highByte(vsync_start));
      self.writeReg(RA8875_VPWR, RA8875_VPWR_LOW + vsync_pw - 1);
      --
      Ada.Text_IO.Put_Line("Configuring RA8875 active window X");
      self.writeReg(RA8875_HSAW0, 0);
      self.writeReg(RA8875_HSAW1, 0);
      self.writeReg(RA8875_HEAW0, lowByte(800 - 1));
      self.writeReg(RA8875_HEAW1, highByte(800 - 1));
      --
      Ada.Text_IO.Put_Line("Configuring RA8875 active window Y");
      self.writeReg(RA8875_VSAW0, 0);
      self.writeReg(RA8875_VSAW1, 0);
      self.writeReg(RA8875_VEAW0, lowByte(480 - 1));
      self.writeReg(RA8875_VEAW1, highByte(480 - 1));
      --
      self.writeReg(RA8875_MCLR, RA8875_MCLR_START or RA8875_MCLR_FULL);
      delay 0.5;
   end;
   --
   procedure set_display(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_PWRR, RA8875_PWRR_NORMAL or RA8875_PWRR_DISPON);
      else
         self.writeReg(RA8875_PWRR, RA8875_PWRR_NORMAL or RA8875_PWRR_DISPOFF);
      end if;
   end;
   --
   procedure set_sleep(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_PWRR, RA8875_PWRR_DISPOFF or RA8875_PWRR_SLEEP);
      else
         self.writeReg(RA8875_PWRR, RA8875_PWRR_DISPOFF);
      end if;
   end;
   --
   procedure GPIOX(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_GPIOX, 1);
      else
         self.writeReg(RA8875_GPIOX, 0);
      end if;
   end;
   --
   procedure graphicsMode(self : RA8875_record) is
      temp : uint8;
   begin
      self.writeCmd(RA8875_MWCR0);
      temp := self.readData;
      temp := temp and not RA8875_MWCR0_TXTMODE;
      self.writeData(temp);
   end;
   --
   procedure textMode(self : RA8875_record) is
      temp : uint8;
   begin
      --
      -- Set mode
      self.writeCmd(RA8875_MWCR0);
      temp := self.readData;
      temp := temp or RA8875_MWCR0_TXTMODE;
      self.writeData(temp);
      --
      -- Select internal font
      self.writeCmd(RA8875_FNCR0);
      temp := self.readData;
      temp := temp and not (128 + 32);
      self.writeData(temp);
   end;
   --
   procedure textColor(self : RA8875_record; bg : R5G6B5_color; fg : R5G6B5_color) is
      temp : uint8;
   begin
      -- Set Fore Color
      self.writeReg(RA8875_FGCR0, fg.R);
      self.writeReg(RA8875_FGCR1, fg.G);
      self.writeReg(RA8875_FGCR2, fg.B);
      --
      -- Set Background Color
      self.writeReg(RA8875_BGCR0, bg.R);
      self.writeReg(RA8875_BGCR1, bg.G);
      self.writeReg(RA8875_BGCR2, bg.B);
      --
      -- Clear transparency flag
      self.writeCmd(RA8875_FNCR1);
      temp := self.readData;
      temp := temp and not 64;
      self.writeData(temp);
   end;
   --
   procedure textSetCursor(self : RA8875_record; x : uint16; y : uint16) is
   begin
      self.writeReg(RA8875_F_CURXL, lowByte(x));
      self.writeReg(RA8875_F_CURXH, highByte(x));
      self.writeReg(RA8875_F_CURYL, lowByte(y));
      self.writeReg(RA8875_F_CURYH, highByte(y));
   end;
   --
   procedure textWrite(self : RA8875_record; str : string) is
   begin
      self.writeCmd(RA8875_MRWC);
      for temp of str loop
         self.writeData(uint8(character'pos(temp)));
         delay 0.001;
      end loop;
   end;
   --
   procedure drawRect(self : RA8875_record; x : uint16; y : uint16; w : uint16;
                      h : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x));
      self.writeReg(RA8875_DLHSR1, highByte(x));
      --
      self.writeReg(RA8875_DLVSR0, lowByte(y));
      self.writeReg(RA8875_DLVSR1, highByte(y));
      --
      self.writeReg(RA8875_DLHER0, lowByte(w));
      self.writeReg(RA8875_DLHER1, highByte(w));
      --
      self.writeReg(RA8875_DLVER0, lowByte(h));
      self.writeReg(RA8875_DLVER1, highByte(h));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      self.writeCmd(RA8875_DCR);
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWSQUARE
                      or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWSQUARE);
      end if;
      --
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
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

end;
