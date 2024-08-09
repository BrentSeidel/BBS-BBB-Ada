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
