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
package body BBS.embed.LED is

   procedure open(l : in led_num) is
   begin
      Ada.Text_IO.Open(led_files(l), Ada.Text_IO.Out_File, led_names(l));
   end;

   procedure open is
   begin
      open(0);
      open(1);
      open(2);
      open(3);
   end;

   procedure set(l : in led_num; s : in led_state) is
   begin
      if s = off then
         Ada.Text_IO.Put(led_files(l), "0");
      else
         Ada.Text_IO.Put(led_files(l), "1");
      end if;
      Ada.Text_IO.Flush(led_files(l));
   end;

   procedure set(led0 : in led_state; led1 : in led_state;
                 led2 : in led_state; led3 : in led_state) is
   begin
      set(0, led0);
      set(1, led1);
      set(2, led2);
      set(3, led3);
   end;

   procedure set(leds : in led_states) is
   begin
      set(0, leds(0));
      set(1, leds(1));
      set(2, leds(2));
      set(3, leds(3));
   end;

   procedure close(l : in led_num) is
   begin
      Ada.Text_IO.Close(led_files(l));
   end;

   procedure close is
   begin
      Ada.Text_IO.Close(led_files(0));
      Ada.Text_IO.Close(led_files(1));
      Ada.Text_IO.Close(led_files(2));
      Ada.Text_IO.Close(led_files(3));
   end;

end;
