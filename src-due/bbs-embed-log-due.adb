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
with BBS.embed.due.serial.int;
package body BBS.embed.log.due is
   --
   --  The basic log does nothing so enabling or disabling it really has no effect.
   --
   procedure enable(self : in out due_log_record) is
   begin
      self.enabled := True;
   end;
   --
   procedure disable(self : in out due_log_record) is
   begin
      self.enabled := False;
   end;
   --
   --  Write log output.
   --
   procedure put(self : due_log_record; text : String) is
   begin
      if self.enabled then
         BBS.embed.due.serial.int.put(0, text);
      end if;
   end;
   --
   procedure put_line(self : due_log_record; text : String) is
   begin
      if self.enabled then
         BBS.embed.due.serial.int.put_line(0, text);
      end if;
   end;
   --
end BBS.embed.log.due;
