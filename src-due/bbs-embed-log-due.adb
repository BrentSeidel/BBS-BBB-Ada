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
