--
--  Author: Brent Seidel
--  Date: 24-Sep-2024
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
package body BBS.embed.GPIO.Linux is
   --
   --  Configure a new GPIO object.
   --
   procedure configure(self : in out Linux_GPIO_record; pin : gpio_id; dir : direction) is
      name : String := names(pin.chip);
   begin
      self.chip := pin.chip;
      self.line := pin.line;
      self.dir  := dir;
      if not gpiochips(self.chip).open then
         gpiochips(self.chip).chip := c_open(name, O_RDONLY);
         if gpiochips(self.chip).chip = -1 then
            gpiochips(self.chip).open := False;
            self.valid := False;
         else
            gpiochips(self.chip).open := True;
            self.valid := True;
         end if;
      end if;
   end;
   --
   --  Set the direction of a pin.  This can be used whether a GPIO pin
   --  has been configured or not.  It is also used by the configure
   --  procedure.
   --
   procedure set_dir(self : in out Linux_GPIO_record;
                     port : String; dir : direction) is
   begin
      null;
   end;
   --
   --  Set the value of an output GPIO.
   --
   overriding
   procedure set(self : Linux_GPIO_record; value : bit) is
   begin
      null;
   end;
   --
   --  Read the value of an input GPIO.
   --
   overriding
   function get(self : Linux_GPIO_record) return bit is
   begin
      return 0;
   end;
   --
   --  Close the file for the pin.  Once this is called, the GPIO object will
   --  need to be re-configured.
   --
   procedure close(self : in out Linux_GPIO_record) is
   begin
      null;
   end;
end BBS.embed.GPIO.Linux;
