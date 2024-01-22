with BBS.embed.log;
package body BBS.embed.i2c.linux is
   --
   -- This works with Debian Jessy for the Beaglebone Black.  The configuration
   -- system has changed and it will probably not work on earlier versions.
   --
   --  This has been updated to work with Ubuntu 21.04 (Hirsute Huppo) on
   --  a Raspberry PI 4 using Linux kernal 5.11.0-1015-raspi.  For some
   --  reason, using the i2c_rdwr option for writing no longer works
   --  properly.  This is odd since the read option still works and is a
   --  more complex transaction.  The object oriented write has been up
   --  dated to use i2c_slave to set the device address and then C_write
   --  to write the actual data (including the register number if
   --  applicable).  The old non-object oriented routines have not been
   --  updated as they are considered to be obsolete.
   --
   -- A status ioctl call produced the following options for the i2c bus.
   --
   -- On my system, this produces    16#efe_000d#
   -- I2C_FUNC_I2C                    0x0000_0001 set
   -- I2C_FUNC_10BIT_ADDR             0x0000_0002
   -- I2C_FUNC_PROTOCOL_MANGLING      0x0000_0004 set
   -- I2C_FUNC_SMBUS_PEC              0x0000_0008 set
   -- I2C_FUNC_NOSTART                0x0000_0010
   -- I2C_FUNC_SMBUS_BLOCK_PROC_CALL  0x0000_8000
   -- I2C_FUNC_SMBUS_QUICK            0x0001_0000
   -- I2C_FUNC_SMBUS_READ_BYTE        0x0002_0000 set
   -- I2C_FUNC_SMBUS_WRITE_BYTE       0x0004_0000 set
   -- I2C_FUNC_SMBUS_READ_BYTE_DATA   0x0008_0000 set
   -- I2C_FUNC_SMBUS_WRITE_BYTE_DATA  0x0010_0000 set
   -- I2C_FUNC_SMBUS_READ_WORD_DATA   0x0020_0000 set
   -- I2C_FUNC_SMBUS_WRITE_WORD_DATA  0x0040_0000 set
   -- I2C_FUNC_SMBUS_PROC_CALL        0x0080_0000 set
   -- I2C_FUNC_SMBUS_READ_BLOCK_DATA  0x0100_0000
   -- I2C_FUNC_SMBUS_WRITE_BLOCK_DATA 0x0200_0000 set
   -- I2C_FUNC_SMBUS_READ_I2C_BLOCK   0x0400_0000 set
   -- I2C_FUNC_SMBUS_WRITE_I2C_BLOCK  0x0800_0000 set
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
   procedure write(addr : addr7; reg : uint8; data : uint8; error : out err_code) is
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
            BBS.embed.log.debug.Put("I2C: Write error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
      end if;
   end;
   --
   -- Function to read a byte from a register on a device on the i2c bus.
   --
   function read(addr : addr7; reg : uint8; error : out err_code)
                       return uint8 is
      status : interfaces.C.int;
      err : integer;
   begin
      --
      --  Message 1 (write the register address)
      --
      msg(0).addr  := uint16(addr);
      msg(0).flags := 0;  -- Write
      msg(0).len   := 1;
      msg(0).buff  := buff1'Access;
      buff1(0) := reg;
      --
      --  Message 2 (read data)
      --
      msg(1).addr  := uint16(addr);
      msg(1).flags := 1;  -- Read
      msg(1).len   := 1;
      msg(1).buff  := buff2'Access;
      --
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Read error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
      end if;
      return buff2(0);
   end;
   --
   -- Function to read a 16 bit word from a register on a device on the i2c bus.
   -- MSB first
   --
   function readm1(addr : addr7; reg : uint8; error : out err_code)
                       return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      --
      --  Message 1 (write the register address)
      --
      msg(0).addr  := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len   := 1;
      msg(0).buff  := buff1'Access;
      buff1(0) := reg;
      --
      --  Message 2 (read data)
      --
      msg(1).addr  := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len   := 2;
      msg(1).buff  := buff2'Access;
      --
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Read error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
      end if;
      return uint16(buff2(0))*256 + uint16(buff2(1));
   end;
   --
   -- Function to read a 16 bit word from a register on a device on the i2c bus.
   -- LSB first
   --
   function readm2(addr : addr7; reg : uint8; error : out err_code)
                       return uint16 is
      status : interfaces.C.int;
      err : integer;
   begin
      --
      --  Message 1 (write the register address)
      --
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      buff1(0) := reg;
      --
      --  Message 2 (read data)
      --
      msg(1).addr := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len := 2;
      msg(1).buff := buff2'Access;
      --
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Read error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
      end if;
      return uint16(buff2(0)) + uint16(buff2(1))*256;
   end;
   --
   -- Procedure to read an arbitrary number of bytes from a device on the i2c bus
   --
   procedure read(addr : addr7; reg : uint8; buff : buff_ptr; size : uint16;
                  error : out err_code) is
      status : interfaces.C.int;
      err : integer;
   begin
      --
      --  Message 1 (write the register address)
      --
      msg(0).addr := uint16(addr);
      msg(0).flags := 0; -- write
      msg(0).len := 1;
      msg(0).buff := buff1'Access;
      buff1(0) := reg;
      --
      --  Message 2 (read data)
      --
      msg(1).addr := uint16(addr);
      msg(1).flags := 1; -- read
      msg(1).len := size;
      msg(1).buff := cbuff_ptr(buff);
      --
      ioctl_msg.messages := msg'Access;
      ioctl_msg.nmsgs := 2;
      status := rdwr_ioctl(i2c_fd, i2c_rdwr, ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Read error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
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
   procedure configure(self : in out linux_i2c_interface_record; i2c_file : string;
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
      self.ioctl_msg.messages := self.msg'Unchecked_Access;
   end;
   --
   procedure configure(self : in out linux_i2c_interface_record; i2c_file : string) is
      ctrl_file : Ada.Text_IO.File_Type;
   begin
      BBS.embed.log.info.put_line("I2C: Configuring I2C interface on " & i2c_file);
      self.port := C_open(i2c_file, O_RDWR, 8#666#);
      self.ioctl_msg.messages := self.msg'Unchecked_Access;
      BBS.embed.log.info.put_line("I2C: Configuration complete.");
   end;
   --
   --  Write a single byte to a specific register
   --
   procedure write(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code) is
   begin
      self.b(0) := data;
      self.write(addr, reg, buff_index(1), error);
   end;
   --
   -- Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                   size : buff_index; error : out err_code) is
      status : interfaces.C.int;
      err : integer;
   begin
--      self.msg(0).addr  := uint16(addr);
--      self.msg(0).flags := 0; -- write
--      self.msg(0).len   := uint16(size + 1);
--      self.msg(0).buff  := self.buff1'Unchecked_Access;
      --
      --  Build the data buffer including the register address in the
      --  first byte.
      --
      self.buff1(0) := reg;
      for x in 0 .. size loop
         self.buff1(buff_index(x + 1)) := self.b(buff_index(x));
      end loop;
      --
--      ioctl_msg.messages := msg'Access;
--      ioctl_msg.nmsgs := 1;
      reset_errno;
--      status := rdwr_ioctl(self.port, i2c_rdwr, ioctl_msg);
--      if integer(status) < 0 then
      --
      --  For some reasion i2c_rdwr doesn't seem to work anymore for writing
      --  data.
      --
      --  Use i2c_slave to set the slave device address
      --
      status := Interfaces.C.int(basic_ioctl(self.port, i2c_slave, Interfaces.C.long(addr)));
      if Integer(status) < 0 then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Write multi error " & Integer'Image(err) &
                                    " occured while setting address.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
         return;
      end if;
      --
      --  Use C_write to write the data buffer.
      --
      reset_errno;
      status := Interfaces.C.int(C_write(self.port, self.buff1, size_t(size + 1)));
      if Integer(status) /= Integer(size + 1) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Write multi error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
            BBS.embed.log.debug.Put_Line("I2C: Wrote " & Integer'Image(Integer(status)) & " bytes");
            BBS.embed.log.debug.Put_Line("I2C: Expected " & Integer'Image(Integer(size + 1)) & " bytes");
         end if;
         error := failed;
      else
         error := none;
      end if;
   end;
   --
   function read(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint8 is
   begin
      self.read(addr, reg, 1, error);
      return self.b(0);
   end;
   --
   function readm1(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint16 is
   begin
      self.read(addr, reg, 2, error);
      return uint16(self.b(0))*256 + uint16(self.b(1));
   end;
   --
   function readm2(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint16 is
   begin
      self.read(addr, reg, 2, error);
      return uint16(self.b(0)) + uint16(self.b(1))*256;
   end;
   --
   procedure read(self : in out linux_i2c_interface_record; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code) is
      status : interfaces.C.int;
      err    : integer;
   begin
      --
      --  Message 1 (write the register address)
      --
      self.msg(0).addr  := uint16(addr);
      self.msg(0).flags := 0; -- write
      self.msg(0).len   := 1;
      self.msg(0).buff  := self.buff1'Unchecked_Access;
      self.buff1(0) := reg;
      --
      --  Message 2 (read data)
      --
      self.msg(1).addr  := uint16(addr);
      self.msg(1).flags := 1; -- read
      self.msg(1).len   := uint16(size);
      self.msg(1).buff  := self.b'Unchecked_Access;
      --
      ioctl_msg.messages := msg'Access;
      self.ioctl_msg.nmsgs := 2;
      status := rdwr_ioctl(self.port, i2c_rdwr, self.ioctl_msg);
      if (integer(status) < 0) then
         err := get_errno;
         if (debug) then
            BBS.embed.log.debug.Put("I2C: Read error " & Integer'Image(err) & " occured.  ");
            BBS.embed.log.debug.Put_Line(cvt_cstr_adastr(strerror(err)));
         end if;
         error := failed;
      else
         error := none;
      end if;
   end;
   --
end;
