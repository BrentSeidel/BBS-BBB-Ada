package body BBS.embed.due.pio is

   --
   --  Configures a pin to be controlled by the PIO controller.  Output is
   --  enabled or disabled based on the value of dir.
   --
   procedure config(self : not null access gpio_record'class;
                    pin : gpio_record; dir : direction) is
   begin
      self.ctrl := pin.ctrl;
      self.bit  := pin.bit;
      self.config(dir);
   end;
   --
   procedure config(self : not null access gpio_record'class; dir : direction) is
   begin
      self.ctrl.PER.Arr(self.bit) := 1;
      if (dir = funct_a) or (dir = funct_b) then
         self.ctrl.PDR.Arr(self.bit) := 1;
         --  OER
         --  ODR
         self.ctrl.OER.Arr(self.bit) := 1;
         --  IFER
         --  IFDR
         self.ctrl.IFDR.Arr(self.bit) := 1;
         --  SODR
         --  CODR
         --  IER
         --  IDR
         self.ctrl.IDR.Arr(self.bit) := 1;
         --  MDER
         --  MDDR
         self.ctrl.MDDR.Arr(self.bit) := 1;
         --  PUDR
         --  PUER
         self.ctrl.PUDR.Arr(self.bit) := 1;
         --  ABSR
         if (dir = funct_a) then
            self.ctrl.ABSR.Arr(self.bit) := 0;
         else
            self.ctrl.ABSR.Arr(self.bit) := 1;
         end if;
         --  OWER
         --  OWDR
         self.ctrl.OWDR.Arr(self.bit) := 1;
      else
         self.ctrl.PER.Arr(self.bit) := 1;
         if dir = gpio_output then
            self.ctrl.OER.Arr(self.bit) := 1;
         else
            self.ctrl.ODR.Arr(self.bit) := 1;
         end if;
      end if;
   end;
   --
   --  Set a pin to a high or low value.
   --
   procedure set(self : not null access gpio_record'class; val : SAM3x8e.Bit) is
   begin
      if val = 1 then
         self.ctrl.SODR.Arr(self.bit) := 1;
      else
         self.ctrl.CODR.Arr(self.bit) := 1;
      end if;
   end;
   --
   --  Read the value of a pin regardless of what is controlling it
   --
   function get(self : not null access gpio_record'class) return SAM3x8e.Bit is
   begin
      return self.ctrl.PDSR.Arr(self.bit);
   end;
   --
   --
   --  Enable or disable pullup on a pin
   --
   procedure pullup(self : not null access gpio_record'class; val : SAM3x8e.Bit) is
   begin
      if val = 1 then
         self.ctrl.PUER.Arr(self.bit) := 1;
      else
         self.ctrl.PUDR.Arr(self.bit) := 1;
      end if;
   end;
   --
end BBS.embed.due.pio;
