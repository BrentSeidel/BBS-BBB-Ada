
package body BBS.embed.due.serial.polled is
   --
   --  Very simple procedure to write a character to the UART.  It does a
   --  busy wait on the UART_SR TXRDY (transmit ready) bit.  It does a loop
   --  until the value of the bit is 1 and then write the character.
   --
   procedure put(c : Character) is
   begin
      put(0, c);
   end;
   --
   procedure put(chan : port_id; c : Character) is
   begin
      while not tx_ready(chan) loop
         null;
      end loop;
      channel(chan).port.THR.TXCHR := Character'Pos(c);
   end;
   --
   --  Procedure to put a string to the serial port
   --
   procedure put(s : string) is
   begin
      for i in s'Range loop
         put(s(i));
      end loop;
   end;
   --
   procedure put(chan : port_id; s : string) is
   begin
      for i in s'Range loop
         put(chan, s(i));
      end loop;
   end;
   --
   --  Procedure to put a string to the serial port followed by a CR/LF
   --
   procedure put_line(s : string) is
   begin
      for i in s'Range loop
         put(s(i));
      end loop;
      put(CR);
      put(LF);
   end;
   --
   procedure put_line(chan : port_id; s : string) is
   begin
      for i in s'Range loop
         put(chan, s(i));
      end loop;
      put(chan, CR);
      put(chan, LF);
   end;
   --
   --  Read a character from serial port - wait for one to be present, if
   --  necessary.  This is not recommended since characters can easily be lost.
   --
   function get return Character is
   begin
        return get(0);
   end;
   --
   function get(chan : port_id) return Character is
      c : Character;
   begin
      while not rx_ready(chan) loop
         null;
      end loop;
      c := Character'Val(channel(chan).port.RHR.RXCHR);
      return c;
   end;

end BBS.embed.due.serial.polled;
