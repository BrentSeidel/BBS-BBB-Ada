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

package body BBS.embed.log.linux is
   --
   --  Define the procedures that can act on a log.
   --
   procedure enable(self : in out linux_log_record) is
   begin
      self.enabled := True;
   end;
   --
   procedure disable(self : in out linux_log_record) is
   begin
      self.enabled := False;
   end;
   --
   --  Write log output.
   --
   procedure put(self : linux_log_record; text : String) is
   begin
      if self.enabled then
         Ada.Text_IO.Put(text);
      end if;
   end;
   --
   procedure put_line(self : linux_log_record; text : String) is
   begin
      if self.enabled then
         Ada.Text_IO.Put_Line(text);
      end if;
   end;
   --
end BBS.embed.log.linux;
