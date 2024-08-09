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
with System;
with SAM3x8e.PMC;  --  Needed to enable clocking
package body BBS.embed.GPIO.Due is

   --
   --  Configures a pin to be controlled by the PIO controller.  Output is
   --  enabled or disabled based on the value of dir.
   --
   procedure config(self : in out Due_GPIO_record;
                    pin : Due_GPIO_record; dir : direction) is
   begin
      self.ctrl := pin.ctrl;
      self.bit  := pin.bit;
      self.config(dir);
   end;
   --
   procedure config(self : in out Due_GPIO_record; dir : direction) is
      pid : Integer := 11; -- Peripheral Identifier
   begin
      --
      --  Determine which PIO controller is being used and enable the clock
      --
      if self.ctrl = BBS.embed.GPIO.Due.PIOA'Access then
         pid := 11;
      elsif self.ctrl = BBS.embed.GPIO.Due.PIOB'Access then
         pid := 12;
      elsif self.ctrl = BBS.embed.GPIO.Due.PIOC'Access then
         pid := 13;
      elsif self.ctrl = BBS.embed.GPIO.Due.PIOD'Access then
         pid := 14;
      end if;
      SAM3x8e.PMC.PMC_Periph.PMC_PCER0.PID.Arr(pid) := 1;
      --
      --  Other initializations
      --
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
   procedure set(self : Due_GPIO_record; val : Bit) is
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
   function get(self : Due_GPIO_record) return Bit is
   begin
      return Bit(self.ctrl.PDSR.Arr(self.bit));
   end;
   --
   --
   --  Enable or disable pullup on a pin
   --
   procedure pullup(self : Due_GPIO_record; val : Bit) is
   begin
      if val = 1 then
         self.ctrl.PUER.Arr(self.bit) := 1;
      else
         self.ctrl.PUDR.Arr(self.bit) := 1;
      end if;
   end;
   --
end BBS.embed.GPIO.Due;
