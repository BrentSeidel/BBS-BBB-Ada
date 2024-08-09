package BBS.embed.log.due is
   --
   type due_log_record is new log_record with private;
   type due_log_ptr is access all due_log_record'Class;
   --
   --  The basic log does nothing so enabling or disabling it really has no effect.
   --
   procedure enable(self : in out due_log_record);
   procedure disable(self : in out due_log_record);
   --
   --  Write log output.
   --
   overriding
   procedure put(self : due_log_record; text : String);
   --
   overriding
   procedure put_line(self : due_log_record; text : String);

private
   type due_log_record is new log_record with
      record
         null;
      end record;
end BBS.embed.log.due;
