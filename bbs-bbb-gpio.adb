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
      -- Open the GPIO file
      --
      if (dir = input) then
         Char_IO.Open(self.gpio_file, Char_IO.In_File, port & "value");
      else
         Char_IO.Open(self.gpio_file, Char_IO.Out_File, port & "value");
      end if;
   end;
   --
   procedure set(self : not null access GPIO_record'class; value : bit) is
   begin
      if (value = 0) then
         Char_IO.Write(self.gpio_file, '0', 1);
      else
         Char_IO.Write(self.gpio_file, '1', 1);
      end if;
   end;
   --
   function get(self : not null access GPIO_record'class) return bit is
      char : character;
   begin
      Char_IO.Read(self.gpio_file, char, 1);
      if (char = '0') then
         return 0;
      else
         return 1;
      end if;
   end;
   --
   procedure close(self : not null access GPIO_record'class) is
   begin
      Char_IO.Close(self.gpio_file);
   end;
end;
