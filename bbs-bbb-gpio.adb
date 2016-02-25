package body BBS.BBB.GPIO is

   function gpio_new return GPIO is
   begin
      return new GPIO_record;
   end;
   --
   procedure configure(self : not null access GPIO_record'class;
                       pin : string; port : string; dir : direction) is
      temp : Ada.Text_IO.File_Type;
   begin
      --
      -- Set pin function
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, pin);
      Ada.Text_IO.Put_Line(temp, "gpio");
      Ada.Text_IO.Close(temp);
      --
      -- Set direction
      --
      Ada.Text_IO.Open(temp, Ada.Text_IO.Out_File, port & "direction");
      if (dir = input) then
         Ada.Text_IO.Put_Line(temp, "in");
      else
         Ada.Text_IO.Put_Line(temp, "out");
      end if;
      Ada.Text_IO.Close(temp);
      self.dir := dir;
      --
      -- Open the output file
      --
      Ada.Text_IO.Open(self.gpio_file, Ada.Text_IO.Out_File, port & "value");
   end;
   --
   procedure set(self : not null access GPIO_record'class; value : bit) is
   begin
      if (value = 0) then
         Ada.Text_IO.Put(self.gpio_file, "0");
      else
         Ada.Text_IO.Put(self.gpio_file, "1");
      end if;
      Ada.Text_IO.Flush(self.gpio_file);
   end;
   --
   function get(self : not null access GPIO_record'class) return bit is
   begin
      return 0;
   end;
   --
   procedure close(self : not null access GPIO_record'class) is
   begin
      Ada.Text_IO.Close(self.gpio_file);
   end;
end;
