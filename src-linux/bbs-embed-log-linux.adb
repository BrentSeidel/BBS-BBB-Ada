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
