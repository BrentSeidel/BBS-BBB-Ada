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
   -- Setup the RA8875 object.  Note that the RA8875 has a hardware reset line.
   -- This line is optional.  Setup functions are provided for configurations
   -- with and without hardware reset.
   --
   -- Setup without hardware reset
   --
   procedure setup(self : in out RA8875_record; CS : BBS.embed.GPIO.GPIO; screen : SPI_ptr) is
   begin
      self.cs_gpio := CS;
      self.reset_gpio := null;
      self.lcd_screen := screen;
      self.cs_gpio.set(gpio_high);
   end;
   --
   -- Setup with hardware reset
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
   -- Toggle the hardware reset line if it has been configured, otherwise do
   -- nothing.
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
   -- Send commands to perform a software reset.
   --
   procedure swReset(self : in out RA8875_record) is
      temp : uint8;
   begin
      self.writeCmd(RA8875_PWRR);
      temp := self.readData;
      temp := temp or RA8875_PWRR_SOFTRESET;
      self.writeData(temp);
      delay 0.1;
      self.writeCmd(RA8875_PWRR);
      temp := self.readData;
      temp := temp and not RA8875_PWRR_SOFTRESET;
      self.writeData(temp);
   end;
   --
   -- The following set of routines are the low level interface to read from and
   -- write to the RA8875.
   --
   procedure writeCmd(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(RA8875_CMDWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(gpio_high);
   end;
   --
   procedure writeData(self : RA8875_record; value : uint8) is
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(RA8875_DATAWRITE);
      self.lcd_screen.set(value);
      self.cs_gpio.set(gpio_high);
   end;
   --
   function readStatus(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(RA8875_CMDREAD);
      temp := self.lcd_screen.get;
      self.cs_gpio.set(gpio_high);
      return temp;
   end;
   --
   function readData(self : RA8875_record) return uint8 is
      temp : uint8;
   begin
      self.cs_gpio.set(gpio_low);
      self.lcd_screen.set(RA8875_DATAREAD);
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
   -- Configure PWM unit 1.  The AdaFruit breakout board uses this to control the
   -- backlight
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
   -- Configure PWM unit 2.
   procedure PWM2config(self : RA8875_record; state : boolean; clock : uint8) is
   begin
      if (state) then
         self.writeReg(RA8875_P2CR, RA8875_PWMCR_ENABLE or (clock and 16#0F#));
      else
         self.writeReg(RA8875_P2CR, RA8875_PWMCR_DISABLE or (clock and 16#0F#));
      end if;
   end;
   --
   -- Set PWM unit 1 output value.  On the AdaFruit breakout board, this sets the
   -- brightness of the backlight.
   --
   procedure PWM1out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(RA8875_P1DCR, value);
   end;
   --
   -- Set the PWM unit 2 output value.
   procedure PWM2out(self : RA8875_record; value : uint8) is
   begin
      self.writeReg(RA8875_P2DCR, value);
   end;
   --
   -- Configure the basic RA8875 settings.
   --
   procedure configure(self : in out RA8875_record; size : RA8875_sizes) is
      pixclk : uint8;
      hsync_start : uint8;
      hsync_pw : uint8;
      hsync_finetune : uint8;
      hsync_nondisp : uint8;
      vsync_pw : uint8;
      vsync_nondisp : uint16;
      vsync_start : uint16;
   begin
      case size is
         when RA8875_480x272 =>
            self.writeReg(RA8875_PLLC1, RA8875_PLLC1_PLLDIV1 + 10);
            delay 0.001;
            self.writeReg(RA8875_PLLC2, RA8875_PLLC2_DIV4);
            delay 0.001;
            self.writeReg(RA8875_SYSR, RA8875_SYSR_16BPP or RA8875_SYSR_MCU8);
            --
            pixclk          := RA8875_PCSR_PDATL or RA8875_PCSR_4CLK;
            hsync_nondisp   := 10;
            hsync_start     := 8;
            hsync_pw        := 48;
            hsync_finetune  := 0;
            vsync_nondisp   := 3;
            vsync_start     := 8;
            vsync_pw        := 10;
            self.width := 480;
            self.height := 272;
         when RA8875_800x480 =>
            self.writeReg(RA8875_PLLC1, RA8875_PLLC1_PLLDIV1 + 10);
            delay 0.001;
            self.writeReg(RA8875_PLLC2, RA8875_PLLC2_DIV4);
            delay 0.001;
            self.writeReg(RA8875_SYSR, RA8875_SYSR_16BPP or RA8875_SYSR_MCU8);
            --
            pixclk          := RA8875_PCSR_PDATL or RA8875_PCSR_2CLK;
            hsync_nondisp   := 26;
            hsync_start     := 32;
            hsync_pw        := 96;
            hsync_finetune  := 0;
            vsync_nondisp   := 32;
            vsync_start     := 23;
            vsync_pw        := 2;
            self.width := 800;
            self.height := 480;
         when others =>
            Ada.Text_IO.Put_Line("RA8875 unknown LCD size.");
            return;
      end case;
      self.cal_top := 0;
      self.cal_bot := self.height;
      self.cal_left := 0;
      self.cal_right := self.width;
      --
      self.writeReg(RA8875_PCSR, pixclk);
      delay 0.001;
      --
      self.writeReg(RA8875_HDWR, uint8((integer(self.width) / 8) - 1));
      self.writeReg(RA8875_HNDFTR, RA8875_HNDFTR_DE_HIGH + hsync_finetune);
      self.writeReg(RA8875_HNDR, (hsync_nondisp - hsync_finetune - 2)/8);
      self.writeReg(RA8875_HSTR, hsync_start/8 - 1);
      self.writeReg(RA8875_HPWR, RA8875_HPWR_LOW + (hsync_pw/8 - 1));
      --
      self.writeReg(RA8875_VDHR0, lowByte(self.height - 1));
      self.writeReg(RA8875_VDHR1, highByte(self.height - 1));
      self.writeReg(RA8875_VNDR0, lowByte(vsync_nondisp - 1));
      self.writeReg(RA8875_VNDR1, highByte(vsync_nondisp));
      self.writeReg(RA8875_VSTR0, lowByte(vsync_start - 1));
      self.writeReg(RA8875_VSTR1, highByte(vsync_start));
      self.writeReg(RA8875_VPWR, RA8875_VPWR_LOW + vsync_pw - 1);
      --
      self.writeReg(RA8875_HSAW0, 0);
      self.writeReg(RA8875_HSAW1, 0);
      self.writeReg(RA8875_HEAW0, lowByte(self.width - 1));
      self.writeReg(RA8875_HEAW1, highByte(self.width - 1));
      --
      self.writeReg(RA8875_VSAW0, 0);
      self.writeReg(RA8875_VSAW1, 0);
      self.writeReg(RA8875_VEAW0, lowByte(self.height - 1));
      self.writeReg(RA8875_VEAW1, highByte(self.height - 1));
      --
      self.writeReg(RA8875_MCLR, RA8875_MCLR_START or RA8875_MCLR_FULL);
      delay 0.5;
   end;
   --
   -- Set the display on or off
   --
   procedure setDisplay(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_PWRR, RA8875_PWRR_NORMAL or RA8875_PWRR_DISPON);
      else
         self.writeReg(RA8875_PWRR, RA8875_PWRR_NORMAL or RA8875_PWRR_DISPOFF);
      end if;
   end;
   --
   -- Set the sleep mode of the display
   --
   procedure setSleep(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_PWRR, RA8875_PWRR_DISPOFF or RA8875_PWRR_SLEEP);
      else
         self.writeReg(RA8875_PWRR, RA8875_PWRR_DISPOFF);
      end if;
   end;
   --
   -- Set the state of the GPIOX pin.  This is used by the AdaFruit breakout board.
   procedure GPIOX(self : RA8875_record; state : boolean) is
   begin
      if (state) then
         self.writeReg(RA8875_GPIOX, 1);
      else
         self.writeReg(RA8875_GPIOX, 0);
      end if;
   end;
   --
   -- Set the bounds for the active window.
   --
   procedure setActiveWindow(self : RA8875_record; top : uint16; bottom : uint16;
                             left : uint16; right : uint16) is
   begin
      self.writeReg(RA8875_HSAW0, lowByte(left));
      self.writeReg(RA8875_HSAW1, highByte(left));
      --
      self.writeReg(RA8875_VSAW0, lowByte(top));
      self.writeReg(RA8875_VSAW1, highByte(top));
      --
      self.writeReg(RA8875_HEAW0, lowByte(right));
      self.writeReg(RA8875_HEAW1, highByte(right));
      --
      self.writeReg(RA8875_VEAW0, lowByte(bottom));
      self.writeReg(RA8875_VEAW1, highByte(bottom));
   end;
   --
   -- Set some of the display control parameters
   --
   procedure setDisplayCtrl(self : RA8875_record; layer : uint8; hdir : uint8;
                            vdir : uint8) is
   begin
      self.writeReg(RA8875_DPCR, layer or hdir or vdir);
   end;
   ----------------------------------------------------------------------------
   -- Text items
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
      temp := temp and not (RA8875_FNCR0_CGRAM or RA8875_FNCR0_EXTCR);
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
   procedure textSetCodePage(self : RA8875_record; page : RA8875_FNCR0_Code_Page) is
      temp : uint8;
   begin
      self.writeCmd(RA8875_FNCR0);
      temp := self.readData;
      temp := (temp and 16#FC#) or uint8(RA8875_FNCR0_Code_Page'pos(page));
      self.writeData(temp);
   end;
   --
   procedure textSetAttribute(self : RA8875_record; align : boolean; transparent : boolean;
                              rotate : boolean; h_size : uint8; v_size : uint8) is
      temp : uint8 := 0;
   begin
      temp := temp or (if (align) then RA8875_FNCR1_ALIGN else 0);
      temp := temp or (if (transparent) then RA8875_FNCR1_TRANS else 0);
      temp := temp or (if (rotate) then RA8875_FNCR1_ROT else 0);
      temp := temp or ((h_size and 16#03#) * 16#04#);
      temp := temp or (v_size and 16#03#);
      self.writeReg(RA8875_FNCR1, temp);
   end;
   --
   procedure textSetLineHeight(self : RA8875_record; size : uint8) is
   begin
      self.writeReg(RA8875_FLDR, size);
   end;
   --
   procedure textSetFontWidth(self : RA8875_record; size : uint8) is
      temp : uint8;
   begin
      self.writeCmd(RA8875_FWTSR);
      temp := self.readData;
      temp := (temp and 16#FC#) or (size and 16#3F#);
      self.writeData(temp);
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
   ---------------------------------------------------------------------------
   -- Graphics items
   --
   -- This section contains routines to access the graphics primitves offered
   -- by the RA8875.
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
   -- Set color for drawing
   --
   procedure drawColor(self : RA8875_record; color : R5G6B5_color) is
      begin
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
   end;
   --
   -- Draw a rectangle
   --
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
      --
      self.writeCmd(RA8875_DCR);
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWSQUARE
                      or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWSQUARE);
      end if;
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
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
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   -- Draw a rectangle with rounded corners
   --
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                         y2 : uint16; rad : uint16; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(rad));
      self.writeReg(RA8875_ELL_A1, highByte(rad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(rad));
      self.writeReg(RA8875_ELL_B1, highByte(rad));
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_SQR
                      or RA8875_ELLIPSE_FILL);
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_SQR);
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
   end;
   --
   procedure drawRndRect(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                         y2 : uint16; rad : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(rad));
      self.writeReg(RA8875_ELL_A1, highByte(rad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(rad));
      self.writeReg(RA8875_ELL_B1, highByte(rad));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_SQR
                      or RA8875_ELLIPSE_FILL);
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_SQR);
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
   end;
   --
   -- Draw a line
   --
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
      --
      self.writeCmd(RA8875_DCR);
      self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWLINE);
      --
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawLine(self : RA8875_record; x1 : uint16; y1 : uint16; x2 : uint16;
                      y2 : uint16; color : R5G6B5_color) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x2));
      self.writeReg(RA8875_DLHER1, highByte(x2));
      self.writeReg(RA8875_DLVER0, lowByte(y2));
      self.writeReg(RA8875_DLVER1, highByte(y2));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      self.writeCmd(RA8875_DCR);
      self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWLINE);
      --
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   -- Draw a circle
   --
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        fill : boolean) is
   begin
      self.writeReg(RA8875_DCHR0, lowByte(x));
      self.writeReg(RA8875_DCHR1, highByte(x));
      self.writeReg(RA8875_DCHV0, lowByte(y));
      self.writeReg(RA8875_DCHV1, highByte(y));
      --
      self.writeReg(RA8875_DCRR, lowByte(rad));
      --
      self.writeCmd(RA8875_DCR);
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_CIRCLE_START or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_CIRCLE_START);
      end if;
      self.waitPoll(RA8875_DCR, RA8875_DCR_CIRCLE_STATUS);
   end;
   --
   procedure drawCircle(self : RA8875_record; x : uint16; y : uint16; rad : uint16;
                        color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DCHR0, lowByte(x));
      self.writeReg(RA8875_DCHR1, highByte(x));
      self.writeReg(RA8875_DCHV0, lowByte(y));
      self.writeReg(RA8875_DCHV1, highByte(y));
      --
      self.writeReg(RA8875_DCRR, lowByte(rad));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      self.writeCmd(RA8875_DCR);
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_CIRCLE_START or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_CIRCLE_START);
      end if;
      self.waitPoll(RA8875_DCR, RA8875_DCR_CIRCLE_STATUS);
   end;
   --
   -- Draw a triangle
   --
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      --
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DTPH0, lowByte(x2));
      self.writeReg(RA8875_DTPH1, highByte(x2));
      --
      self.writeReg(RA8875_DTPV0, lowByte(y2));
      self.writeReg(RA8875_DTPV1, highByte(y2));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x3));
      self.writeReg(RA8875_DLHER1, highByte(x3));
      --
      self.writeReg(RA8875_DLVER0, lowByte(y3));
      self.writeReg(RA8875_DLVER1, highByte(y3));
      --
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWTRIANGLE
                      or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWTRIANGLE);
      end if;
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   procedure drawTriangle(self : RA8875_record; x1 : uint16; y1 : uint16;
                          x2 : uint16; y2 : uint16; x3 : uint16; y3 : uint16;
                          color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DLHSR0, lowByte(x1));
      self.writeReg(RA8875_DLHSR1, highByte(x1));
      --
      self.writeReg(RA8875_DLVSR0, lowByte(y1));
      self.writeReg(RA8875_DLVSR1, highByte(y1));
      --
      self.writeReg(RA8875_DTPH0, lowByte(x2));
      self.writeReg(RA8875_DTPH1, highByte(x2));
      --
      self.writeReg(RA8875_DTPV0, lowByte(y2));
      self.writeReg(RA8875_DTPV1, highByte(y2));
      --
      self.writeReg(RA8875_DLHER0, lowByte(x3));
      self.writeReg(RA8875_DLHER1, highByte(x3));
      --
      self.writeReg(RA8875_DLVER0, lowByte(y3));
      self.writeReg(RA8875_DLVER1, highByte(y3));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWTRIANGLE
                      or RA8875_DCR_FILL);
      else
         self.writeReg(RA8875_DCR, RA8875_DCR_LINESQUTRI_START or RA8875_DCR_DRAWTRIANGLE);
      end if;
      self.waitPoll(RA8875_DCR, RA8875_DCR_LINESQUTRI_STATUS);
   end;
   --
   -- Draw an ellipse
   --
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; fill : boolean) is
   begin
      self.writeReg(RA8875_DEHR0, lowByte(x));
      self.writeReg(RA8875_DEHR1, highByte(x));
      self.writeReg(RA8875_DEVR0, lowByte(y));
      self.writeReg(RA8875_DEVR1, highByte(y));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(hRad));
      self.writeReg(RA8875_ELL_A1, highByte(hRad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(vRad));
      self.writeReg(RA8875_ELL_B1, highByte(vRad));
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_FILL);
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START);
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
   end;
   --
   procedure drawEllipse(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DEHR0, lowByte(x));
      self.writeReg(RA8875_DEHR1, highByte(x));
      self.writeReg(RA8875_DEVR0, lowByte(y));
      self.writeReg(RA8875_DEVR1, highByte(y));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(hRad));
      self.writeReg(RA8875_ELL_A1, highByte(hRad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(vRad));
      self.writeReg(RA8875_ELL_B1, highByte(vRad));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_FILL);
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START);
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
   end;
   --
   -- Draw one of four segments of an ellipse
   --
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; seg : RA8875_ELLIPSE_PART; fill : boolean) is
   begin
      self.writeReg(RA8875_DEHR0, lowByte(x));
      self.writeReg(RA8875_DEHR1, highByte(x));
      self.writeReg(RA8875_DEVR0, lowByte(y));
      self.writeReg(RA8875_DEVR1, highByte(y));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(hRad));
      self.writeReg(RA8875_ELL_A1, highByte(hRad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(vRad));
      self.writeReg(RA8875_ELL_B1, highByte(vRad));
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_CURVE or
                         RA8875_ELLIPSE_FILL or uint8(RA8875_ELLIPSE_PART'pos(seg)));
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_CURVE or
                      uint8(RA8875_ELLIPSE_PART'pos(seg)));
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
   end;
   --
   procedure drawEllipseSegment(self : RA8875_record; x : uint16; y : uint16; hRad : uint16;
                          vRad : uint16; seg : RA8875_ELLIPSE_PART; color : R5G6B5_color; fill : boolean) is
   begin
      self.writeReg(RA8875_DEHR0, lowByte(x));
      self.writeReg(RA8875_DEHR1, highByte(x));
      self.writeReg(RA8875_DEVR0, lowByte(y));
      self.writeReg(RA8875_DEVR1, highByte(y));
      --
      self.writeReg(RA8875_ELL_A0, lowByte(hRad));
      self.writeReg(RA8875_ELL_A1, highByte(hRad));
      --
      self.writeReg(RA8875_ELL_B0, lowByte(vRad));
      self.writeReg(RA8875_ELL_B1, highByte(vRad));
      --
      self.writeReg(RA8875_FGCR0, color.R);
      self.writeReg(RA8875_FGCR1, color.G);
      self.writeReg(RA8875_FGCR2, color.B);
      --
      if (fill) then
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_CURVE or
                         RA8875_ELLIPSE_FILL or uint8(RA8875_ELLIPSE_PART'pos(seg)));
      else
         self.writeReg(RA8875_ELLIPSE, RA8875_ELLIPSE_START or RA8875_ELLIPSE_CURVE or
                      uint8(RA8875_ELLIPSE_PART'pos(seg)));
      end if;
      self.waitPoll(RA8875_ELLIPSE, RA8875_ELLIPSE_STATUS);
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
   -- Touch items
   --
   procedure enableTouch(self : RA8875_record; state : boolean) is
      adcClock : uint8;
      temp : uint8;
   begin
      case self.width is
         when 480 =>
            adcClock := RA8875_TPCR0_ADCCLK_DIV4;
         when 800 =>
            adcClock := RA8875_TPCR0_ADCCLK_DIV16;
         when others =>
            adcClock := RA8875_TPCR0_ADCCLK_DIV4;
      end case;
      if (state) then
         self.writeReg(RA8875_TPCR0, RA8875_TPCR0_ENABLE or RA8875_TPCR0_WAIT_4096CLK or
                    RA8875_TPCR0_WAKEENABLE or adcClock);
         self.writeReg(RA8875_TPCR1, RA8875_TPCR1_AUTO or RA8875_TPCR1_DEBOUNCE);
      self.writeCmd(RA8875_INTC1);
      temp := self.readData;
      temp := temp and RA8875_INTC1_TP;
      self.writeData(temp);
      else
         self.writeReg(RA8875_TPCR0, RA8875_TPCR0_DISABLE);
         self.writeCmd(RA8875_INTC1);
         temp := self.readData;
         temp := temp and not RA8875_INTC1_TP;
         self.writeData(temp);
      end if;
   end;
   --
   function checkTouched(self : RA8875_record) return boolean is
      temp : uint8 := self.readReg(RA8875_INTC2);
   begin
      self.writeReg(RA8875_INTC2, RA8875_INTC2_TP);
      if ((temp and RA8875_INTC2_TP) = 0) then
         return false;
      else
         return true;
      end if;
   end;
   --
   procedure readTouchRaw(self : RA8875_record; x : out uint16; y : out uint16) is
      x_msb : uint8 := self.readReg(RA8875_TPXH);
      y_msb : uint8 := self.readReg(RA8875_TPYH);
      xy_lsb : uint8 := self.readReg(RA8875_TPXYL);
   begin
      x := uint16(x_msb)*4 + uint16(xy_lsb and RA8875_TPXYL_X_LSB);
      y := uint16(y_msb)*4 + uint16(xy_lsb and RA8875_TPXYL_Y_LSB)/4;
   end;
   --
   procedure readTouchCal(self : RA8875_record; x : out uint16; y : out uint16) is
      x_msb : uint8 := self.readReg(RA8875_TPXH);
      y_msb : uint8 := self.readReg(RA8875_TPYH);
      xy_lsb : uint8 := self.readReg(RA8875_TPXYL);
      temp_x : float;
      temp_y : float;
   begin
      temp_x := float(x_msb)*4.0 + float(xy_lsb and RA8875_TPXYL_X_LSB);
      temp_y := float(y_msb)*4.0 + float((xy_lsb and RA8875_TPXYL_Y_LSB)/4);
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
      self.drawRect(0, 0, self.width - 1, self.height - 1, RA8875_BLACK, true);
      self.enableTouch(true);
      -- Find top edge
      self.textMode;
      self.screenActive;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(BBS.embed.SPI.RA8875.RA8875_BLACK, BBS.embed.SPI.RA8875.RA8875_WHITE);
      self.textSetCodePage(BBS.embed.SPI.RA8875.RA8875_FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.textSetCursor(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Top Edge");
      self.graphicsMode;
      self.drawRect(0, 0, self.width - 1, 10, BBS.embed.SPI.RA8875.RA8875_WHITE, true);
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
      self.drawRect(0, 0, self.width - 1, self.height - 1, RA8875_BLACK, true);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(BBS.embed.SPI.RA8875.RA8875_BLACK, BBS.embed.SPI.RA8875.RA8875_WHITE);
      self.textSetCodePage(BBS.embed.SPI.RA8875.RA8875_FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.textSetCursor(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Bottom Edge");
      self.graphicsMode;
      self.drawRect(0, self.height - 10, self.width - 1, self.height - 1, BBS.embed.SPI.RA8875.RA8875_WHITE, true);
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
      self.drawRect(0, 0, self.width - 1, self.height - 1, RA8875_BLACK, true);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(BBS.embed.SPI.RA8875.RA8875_BLACK, BBS.embed.SPI.RA8875.RA8875_WHITE);
      self.textSetCodePage(BBS.embed.SPI.RA8875.RA8875_FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.textSetCursor(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Left Edge");
      self.graphicsMode;
      self.drawRect(0, 0, 10, self.height - 1, BBS.embed.SPI.RA8875.RA8875_WHITE, true);
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
      self.drawRect(0, 0, self.width - 1, self.height - 1, RA8875_BLACK, true);
      self.textMode;
      self.textSetAttribute(true, false, false, 0, 0);
      self.textColor(BBS.embed.SPI.RA8875.RA8875_BLACK, BBS.embed.SPI.RA8875.RA8875_WHITE);
      self.textSetCodePage(BBS.embed.SPI.RA8875.RA8875_FNCR0_ISO8859_1);
      self.textSetAttribute(true, false, false, 3, 3);
      self.textSetCursor(self.width/2 - 7*40, self.height/2 - 10);
      self.textWrite("Touch Right Edge");
      self.graphicsMode;
      self.drawRect(self.width - 10, 0, self.width - 1, self.height - 1, BBS.embed.SPI.RA8875.RA8875_WHITE, true);
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
   -- Miscellaneous items
   --
   -- Define a scroll area and move it
   --
   procedure scroll(self : RA8875_record; hStart : uint16; vStart : uint16;
                    hEnd : uint16; vEnd : uint16; hOffset : uint16; vOffset : uint16) is
   begin
      self.writeReg(RA8875_HSSW0, lowByte(hStart));
      self.writeReg(RA8875_HSSW1, highByte(hStart));
      self.writeReg(RA8875_VSSW0, lowByte(vStart));
      self.writeReg(RA8875_VSSW1, highByte(vStart));
      --
      self.writeReg(RA8875_HEAW0, lowByte(hEnd));
      self.writeReg(RA8875_HEAW1, highByte(hEnd));
      self.writeReg(RA8875_VEAW0, lowByte(vEnd));
      self.writeReg(RA8875_VEAW1, highByte(vEnd));
      --
      if (hOffset /= 0) then
         self.writeReg(RA8875_HOFS0, lowByte(hOffset));
         self.writeReg(RA8875_HOFS1, highByte(hOffset));
      end if;
      if (vOffset /= 0) then
         self.writeReg(RA8875_VOFS0, lowByte(vOffset));
         self.writeReg(RA8875_VOFS1, highByte(vOffset));
      end if;
   end;
   --
   -- Fill the screen with a solid color.
   --
   procedure fillScreen(self : RA8875_record; color : R5G6B5_color) is
   begin
      self.drawRect(0, 0, self.width - 1, self.height - 1, color, true);
   end;
   --
   -- Set the active area to be the whole screen
   --
   procedure screenActive(self : RA8875_record) is
   begin
      self.setActiveWindow(0, self.height - 1, 0, self.width - 1);
   end;
   --



end;
