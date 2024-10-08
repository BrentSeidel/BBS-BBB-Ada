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
--
--  This package provides the definitions for logging capabilities.  Since not
--  all embedded systems have access to Ada.Text_IO, this package is provided.
--  It defines a logging object that can be subclassed to perform logging on
--  the specific target without having to modify the client packages.  This basic
--  class will simply discard any text.  Note that multiple logging streams may
--  be defined.
--

package BBS.embed.log is
   --
   --  Define the log record.  It simply provides a flag indicating whether logging
   --  is enabled or disabled.  Subclasses may add more information as needed.
   --
   type log_record is tagged
      record
         enabled : Boolean;
      end record;
   type log_ptr is access all log_record'Class;
   --
   --  Define the procedures that can act on a log.
   --
   --  The basic log does nothing so enabling or disabling it really has no effect.
   --
   procedure enable(self : in out log_record) is null;
   procedure disable(self : in out log_record) is null;
   --
   --  Write log output.
   --
   procedure put(self : log_record; text : String) is null;
   procedure put_line(self : log_record; text : String) is null;
   --
   --  Define some objects for logging.  Client packages can use these.  The top
   --  level package can set these to the appropriate subtype to support logging.
   --
   dummy_log : aliased log_record;
   --
   --  These are to be used by the client packages for the type of message desired.
   --  In here they all point to dummy_log, but the top level program can reassign
   --  them as needed.
   --
   debug : log_ptr := dummy_log'Access;
   info  : log_ptr := dummy_log'Access;
   error : log_ptr := dummy_log'Access;

end BBS.embed.log;
