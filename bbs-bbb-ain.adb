package body BBS.BBB.AIN is

   function AIN_new return AIN is
   begin
      return new AIN_record;
   end;
   --
   procedure configure(self : not null access AIN_record'class;
                       port : string) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      -- Open the AIN file
      --
      Char_IO.Open(self.AIN_file, Char_IO.In_File, port);
   end;
   --
   function get(self : not null access AIN_record'class) return uint12 is
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
      when Ada.IO_Exceptions.End_Error =>
         return 0;
   end;
   --
   procedure close(self : not null access AIN_record'class) is
   begin
      Char_IO.Close(self.AIN_file);
   end;
end;
