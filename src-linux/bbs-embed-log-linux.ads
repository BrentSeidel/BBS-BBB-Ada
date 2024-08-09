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
package BBS.embed.log.linux is
   --
   --  The linux_log_record is basically just a log_record.
   --
   type linux_log_record is new log_record with
     record
         null;
      end record;
   type linux_log_ptr is access all linux_log_record'Class;
   --
   --  Define the procedures that can act on a log.
   --
   procedure enable(self : in out linux_log_record);
   procedure disable(self : in out linux_log_record);
   --
   --  Write log output.
   --
   procedure put(self : linux_log_record; text : String);
   procedure put_line(self : linux_log_record; text : String);
   --
   --  Define object for logging.
   --
   linux_log : aliased linux_log_record;

end BBS.embed.log.linux;
