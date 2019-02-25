package body BBS.embed.i2c.linux is
   --
   -- This works with Debian Jessy for the Beaglebone Black.  The configuration
   -- system has changed and it will probably not work on earlier versions.
   --
   -- A status ioctl call produced the following options for the i2c bus.
   --
   -- On my system, this produces    16#efe_000d#
   -- I2C_FUNC_I2C			0x0000_0001 set
   -- I2C_FUNC_10BIT_ADDR		0x0000_0002
   -- I2C_FUNC_PROTOCOL_MANGLING	0x0000_0004 set
   -- I2C_FUNC_SMBUS_PEC		0x0000_0008 set
   -- I2C_FUNC_NOSTART		0x0000_0010
   -- I2C_FUNC_SMBUS_BLOCK_PROC_CALL	0x0000_8000
   -- I2C_FUNC_SMBUS_QUICK		0x0001_0000
   -- I2C_FUNC_SMBUS_READ_BYTE	0x0002_0000 set
   -- I2C_FUNC_SMBUS_WRITE_BYTE	0x0004_0000 set
   -- I2C_FUNC_SMBUS_READ_BYTE_DATA	0x0008_0000 set
   -- I2C_FUNC_SMBUS_WRITE_BYTE_DATA	0x0010_0000 set
   -- I2C_FUNC_SMBUS_READ_WORD_DATA	0x0020_0000 set
   -- I2C_FUNC_SMBUS_WRITE_WORD_DATA	0x0040_0000 set
   -- I2C_FUNC_SMBUS_PROC_CALL	0x0080_0000 set
   -- I2C_FUNC_SMBUS_READ_BLOCK_DATA	0x0100_0000
   -- I2C_FUNC_SMBUS_WRITE_BLOCK_DATA 0x0200_0000 set
   -- I2C_FUNC_SMBUS_READ_I2C_BLOCK	0x0400_0000 set
   -- I2C_FUNC_SMBUS_WRITE_I2C_BLOCK	0x0800_0000 set
   --
   procedure configure(i2c_file : string) is
      ctrl_file : Ada.Text_IO.File_Type;
   begin
      --
      -- Set the pins to the proper state.  This may need root access.
      --
      Ada.Text_IO.Open(ctrl_file, Ada.Text_IO.Out_File, SCL_Ctrl);
      Ada.Text_IO.Put_Line(ctrl_file, "i2c");
      Ada.Text_IO.Close(ctrl_file);
      Ada.Text_IO.Open(ctrl_file, Ada.Text_IO.Out_File, SDA_Ctrl);
      Ada.Text_IO.Put_Line(ctrl_file, "i2c");
      Ada.Text_IO.Close(ctrl_file);
      i2c_fd := C_open(i2c_file, O_RDWR, 8#666#);
   end;
   --
   -- Procedure to write a byte to a register on a device on the i2c bus.
   --
   procedure write(addr : addr7; reg : uint8; data : uint8; error : out integer) is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0;
      msg(0).len := 2;
      msg(0).buff := buff1'Access;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 1;
      buff1(0) := reg;
      buff1(1) := data;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Write error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
   end;
   --
   -- Function to read a byte from a register on a device on the i2c bus.
   --
   function read(addr : addr7; reg : uint8; error : out integer)
                       return uint8 is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0;
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      msg(1).addr := uint16(addr);
      msg(1).flags := 1;
      msg(1).len := 1;
      msg(1).buff := buff2'Access;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      buff1(0) := reg;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return buff2(0);
   end;
   --
   -- Function to read a 16 bit word from a register on a device on the i2c bus.
   -- MSB first
   --
   function readm1(addr : addr7; reg : uint8; error : out integer)
                       return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      msg(1).addr := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len := 2;
      msg(1).buff := buff2'Access;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      buff1(0) := reg;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return uint16(buff2(0))*256 + uint16(buff2(1));
   end;
   --
   -- Function to read a 16 bit word from a register on a device on the i2c bus.
   -- LSB first
   --
   function readm2(addr : addr7; reg : uint8; error : out integer)
                       return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      msg(1).addr := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len := 2;
      msg(1).buff := buff2'Access;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      buff1(0) := reg;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return uint16(buff2(0)) + uint16(buff2(1))*256;
   end;
   --
   -- Procedure to read an arbitrary number of bytes from a device on the i2c bus
   --
   procedure read(addr : addr7; reg : uint8; buff : buff_ptr; size : uint16;
                  error : out integer) is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      msg(1).addr := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len := size;
      msg(1).buff := buff;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      buff1(0) := reg;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
   end;
   --
   -- Convert an string from strerror into a printable Ada string
   --
   function cvt_cstr_adastr(str_ptr : err_msg_ptr) return string is
      null_loc : constant integer :=
        Ada.Strings.Fixed.index(string(str_ptr.all), "" & ASCII.NUL);
   begin
      return Ada.Strings.Fixed.head(string(str_ptr.all), null_loc - 1);
   end;
   --
   -- Object oriented interface
   --
   function i2c_new return i2c_interface is
   begin
      return new i2c_interface_record;
   end;
   --
   procedure configure(self : not null access i2c_interface_record'class; i2c_file : string;
                      SCL : string; SDA : string) is
      ctrl_file : Ada.Text_IO.File_Type;
   begin
      --
      -- Set the pins to the proper state.  This may need root access.
      --
      Ada.Text_IO.Open(ctrl_file, Ada.Text_IO.Out_File, SCL);
      Ada.Text_IO.Put_Line(ctrl_file, "i2c");
      Ada.Text_IO.Close(ctrl_file);
      Ada.Text_IO.Open(ctrl_file, Ada.Text_IO.Out_File, SDA);
      Ada.Text_IO.Put_Line(ctrl_file, "i2c");
      Ada.Text_IO.Close(ctrl_file);
      self.port := C_open(i2c_file, O_RDWR, 8#666#);
      self.msg(0).buff := self.buff1'Access;
      self.ioctl_msg.messages := self.msg'Access;
      self.msg(1).buff := self.buff2'Access;
   end;
   --
   procedure configure(self : not null access i2c_interface_record'class; i2c_file : string) is
      ctrl_file : Ada.Text_IO.File_Type;
   begin
      self.port := C_open(i2c_file, O_RDWR, 8#666#);
      self.msg(0).buff := self.buff1'Access;
      self.ioctl_msg.messages := self.msg'Access;
      self.msg(1).buff := self.buff2'Access;
   end;
   --
   procedure write(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                   data : uint8; error : out integer) is
      status : interfaces.C.int;
      err : integer;
   begin
      self.msg(0).addr := uint16(addr);
      self.msg(0).flags := 0;
      self.msg(0).len := 2;
      self.ioctl_msg.nmsgs := 1;
      self.buff1(0) := reg;
      self.buff1(1) := data;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Write error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
   end;
   --
   --
   -- Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                   buff : buff_ptr; size : uint16; error : out integer) is
      status : interfaces.C.int;
      err : integer;
   begin
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := size + 1;
      msg(0).buff := buff1'Access;
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 1;
      buff1(0) := reg;
      for x in 0 .. size loop
         buff1(integer(x + 1)) := buff(integer(x));
      end loop;
      status := rdwr_ioctl(self.port, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Write error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
   end;
   --
   function read(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint8 is
      status : interfaces.C.int;
      err : integer;
   begin
      self.msg(0).addr := uint16(addr);
      self.msg(0).flags := 0;
      self.msg(0).len := 1;
      self.msg(1).addr := uint16(addr);
      self.msg(1).flags := 1;
      self.msg(1).len := 1;
      self.ioctl_msg.nmsgs := 2;
      self.buff1(0) := reg;
      self.msg(1).buff := self.buff2'Access;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return self.buff2(0);
   end;
   --
   function readm1(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      self.msg(0).addr := uint16(addr);
      self.msg(0).flags := 0; -- write
      self.msg(0).len := 1;
      self.msg(1).addr := uint16(addr);
      self.msg(1).flags := 1; -- read
      self.msg(1).len := 2;
      self.msg(1).buff := self.buff2'Access;
      self.ioctl_msg.nmsgs := 2;
      self.buff1(0) := reg;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return uint16(self.buff2(0))*256 + uint16(self.buff2(1));
   end;
   --
   function readm2(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      self.msg(0).addr := uint16(addr);
      self.msg(0).flags := 0; -- write
      self.msg(0).len := 1;
      self.msg(1).addr := uint16(addr);
      self.msg(1).flags := 1; -- read
      self.msg(1).len := 2;
      self.msg(1).buff := self.buff2'Access;
      self.ioctl_msg.nmsgs := 2;
      self.buff1(0) := reg;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
      return uint16(self.buff2(0)) + uint16(self.buff2(1))*256;
   end;
   --
   procedure read(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                  buff : buff_ptr; size : uint16; error : out integer) is
      status : interfaces.C.int;
      err : integer;
   begin
      self.msg(0).addr := uint16(addr);
      self.msg(0).flags := 0; -- write
      self.msg(0).len := 1;
      self.msg(1).addr := uint16(addr);
      self.msg(1).flags := 1; -- read
      self.msg(1).len := size;
      self.msg(1).buff := buff;
      self.ioctl_msg.nmsgs := 2;
      self.buff1(0) := reg;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            Ada.Text_IO.Put("Read error " & Integer'Image(err) & " occured.  ");
            Ada.Text_IO.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := err;
      else
         error := 0;
      end if;
   end;
   --
   function i2c_new return i2c_device is
   begin
      return new i2c_device_record;
   end;
   --
   procedure configure(self : not null access i2c_device_record'Class; port : i2c_interface;
                      addr : addr7) is
   begin
      self.port := port;
      self.address := addr;
   end;
end;
