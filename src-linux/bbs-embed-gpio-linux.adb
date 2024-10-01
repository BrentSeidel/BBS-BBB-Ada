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
      request : gpio_v2_line_request;
      temp    : Interfaces.C.Int;
      fd      : file_id;
   begin
         request.offsets := (others => 0);
         request.offsets(0) := self.offset;
         request.num_lines := 1;
         request.event_buffer_size := 0;
         request.config.num_attrs := 0;
         request.config.flags := (others => False);
         if dir = input then
            request.config.flags.GPIO_V2_LINE_FLAG_INPUT := True;
         else
            request.config.flags.GPIO_V2_LINE_FLAG_OUTPUT := True;
         end if;
         fd := gpiochips(self.chip).chip;
         temp := req_ioctl(fd, GPIO_V2_GET_LINE_IOCTL, request);
      null;
   end;
   --
   --  Set the value of an output GPIO.
   --
   overriding
   procedure set(self : Linux_GPIO_record; value : bit) is
--      values : gpio_v2_line_values;
   begin
      null;
   end;
   --
   --  Read the value of an input GPIO.
   --
   overriding
   function get(self : Linux_GPIO_record) return bit is
      values : gpio_v2_line_values;
      temp   : Interfaces.C.Int;
   begin
      values.bits := (others => 0);
      values.mask := (others => 0);
      values.mask(0) := 1;
      temp := values_ioctl(self.req, GPIO_V2_LINE_GET_VALUES_IOCTL, values);
      return values.bits(Integer(self.line));
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
