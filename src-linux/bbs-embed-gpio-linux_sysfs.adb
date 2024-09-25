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
package body BBS.embed.GPIO.Linux is

   function gpio_new return GPIO is
   begin
      return new Linux_GPIO_record;
   end;
   --
   --  Set the direction of a pin.  This is a helper function that is not
   --  tied to a specific pin record and can be used whether a GPIO pin
   --  has been configured or not.  It is also used by the configure
   --  procedure.
   --
   procedure set_dir(self : in out Linux_GPIO_record;
                     port : String; dir : direction) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      --  Set direction
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, port & "direction");
      if (dir = input) then
         Ada.Text_IO.Put_Line(temp, "in");
      else
         Ada.Text_IO.Put_Line(temp, "out");
      end if;
      Ada.Text_IO.Close(temp);
      self.dir := dir;
      --
      --  Open the GPIO file
      --
      if Char_IO.Is_Open(self.gpio_file) then
         Char_IO.Close(self.gpio_file);
      end if;
      if (dir = input) then
         Char_IO.Open(self.gpio_file, Char_IO.In_File, port & "value");
      else
         Char_IO.Open(self.gpio_file, Char_IO.Out_File, port & "value");
      end if;
   end;
   --
   procedure configure(self : in out Linux_GPIO_record;
                       pin : string; port : string; dir : direction) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      --  Set pin function
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pin);
      Ada.Text_IO.Put_Line(temp, "gpio");
      Ada.Text_IO.Close(temp);
      --
      --  Set active status
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, port & "active_low");
      Ada.Text_IO.Put_Line(temp, "0");
      Ada.Text_IO.Close(temp);
      --
      --  Set direction and open GPIO file
      --
      self.set_dir(port, dir);
      --
      --  Open the GPIO file
      --
--      if (dir = input) then
--         Char_IO.Open(self.gpio_file, Char_IO.In_File, port & "value");
--      else
--         Char_IO.Open(self.gpio_file, Char_IO.Out_File, port & "value");
--      end if;
   end;
   --
   procedure configure(self : in out Linux_GPIO_record;
                       port : string; dir : direction) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      --  Set active status
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, port & "active_low");
      Ada.Text_IO.Put_Line(temp, "0");
      Ada.Text_IO.Close(temp);
      --
      --  Set direction and open GPIO file
      --
      self.set_dir(port, dir);
      --
      --  Open the GPIO file
      --
--      if (dir = input) then
--         Char_IO.Open(self.gpio_file, Char_IO.In_File, port & "value");
--      else
--         Char_IO.Open(self.gpio_file, Char_IO.Out_File, port & "value");
--      end if;
   end;
   --
   procedure set(self : Linux_GPIO_record; value : bit) is
   begin
      if (value = 0) then
         Char_IO.Write(self.gpio_file, '0', 1);
         Char_IO.Write(self.gpio_file, '0', 1);
      else
         Char_IO.Write(self.gpio_file, '1', 1);
         Char_IO.Write(self.gpio_file, '1', 1);
      end if;
   end;
   --
   function get(self : Linux_GPIO_record) return bit is
      char : character;
   begin
      Char_IO.Read(self.gpio_file, char, 1);
      if (char = '0') then
         return 0;
      else
         return 1;
      end if;
   end;
   --
   procedure close(self : in out Linux_GPIO_record) is
   begin
      Char_IO.Close(self.gpio_file);
   end;
end;
