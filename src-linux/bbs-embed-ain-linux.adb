with BBS.embed.log;
package body BBS.embed.AIN.linux is

   function AIN_new return AIN is
   begin
      return new Linux_AIN_record;
   end;
   --
   procedure configure(self : in out Linux_AIN_record;
                       port : string) is
--      temp : Char_IO.File_Type;
   begin
      --
      -- Open the AIN file
      --
      Char_IO.Open(self.AIN_file, Char_IO.In_File, port);
--      self.AIN_file := temp;
   end;
   --
   function get(self : Linux_AIN_record) return uint12 is
      char : character;
      buff : string(1 .. 5) := "     ";
      index : integer := 1;
   begin
      Char_IO.Set_Index(self.AIN_file, 1);
      while (not Char_IO.End_Of_File(self.AIN_file)) loop
         Char_IO.Read(self.AIN_file, char);
         exit when (char = ASCII.LF);
         buff(index) := char;
         index := index + 1;
      end loop;
      return uint12(integer'Value(buff));
   exception
      --
      -- End of file exception should really not occur since that would cause
      -- an immediate exit to the while loop.  I think that it is happening
      -- when the first read has an end of file.  This probably means that the
      -- return value should be zero.
      --
      when Ada.IO_Exceptions.End_Error =>
         return 0;
      --
      -- The 12 bit A/D converter should not be returning values greater than
      -- 4095.  However, I have seen this happen and a constraint exception is
      -- thrown.  This handler just prints out the value of buff and then passes
      -- the exception on.  This provides a little bit of debugging information.
      --
      when Constraint_Error =>
         BBS.embed.log.error.Put_Line("Analog input conversion failed on value <" & buff & ">");
         raise;
   end;
   --
   function get(self : Linux_AIN_record) return BBS.units.emf_v is
      reading : uint12;
   begin
      reading := self.get;
      return max_volts*BBS.units.emf_v(reading)/BBS.units.emf_v(uint12'Last);
   end;
   --
   procedure close(self : in out Linux_AIN_record) is
   begin
      Char_IO.Close(self.AIN_file);
   end;
end;
