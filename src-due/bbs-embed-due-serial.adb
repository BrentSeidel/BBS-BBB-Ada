pragma Warnings (Off);
with System.Sam3x8;
use type System.Sam3x8.Word;
pragma Warnings (On);

with SAM3x8e.PMC;
with SAM3x8e.PIO;
with BBS.embed.due.serial.polled;
package body BBS.embed.due.serial is
   --
   --  The programming USB port is connected to pins PA08 and PA09 - URXD and
   --  UTXD.  These are controlled by the UART.  Include code here to
   --  initialize the UART and set the baud rate.
   --
   procedure init(baud_rate : SAM3x8e.UInt32) is
      master_clock : constant := 84_000_000 / 16;
      baud_constant : constant SAM3x8e.UInt32 := master_clock / baud_rate;
      PA08 : constant SAM3x8e.UInt32 := (2**8);
      PA09 : constant SAM3x8e.UInt32 := (2**9);
      pins : constant SAM3x8e.UInt32 := PA08 or PA09;
   begin
      --
      --  Enable clock for UART
      --
      SAM3x8e.PMC.PMC_Periph.PMC_PCER0.PID.Arr(System.Sam3x8.UART_ID) := 1;
      --
      --  Configure pins PA08 and PA09 to be UART pins.
      --
      --  PER
      --  PDR
      BBS.embed.GPIO.Due.PIOA.PDR.Val := pins;
      --  OER
      --  ODR
      BBS.embed.GPIO.Due.PIOA.ODR.Val := pins;
      --  IFER
      --  IFDR
      BBS.embed.GPIO.Due.PIOA.IFDR.Val := pins;
      --  SODR
      --  CODR
      --  IER
      --  IDR
      BBS.embed.GPIO.Due.PIOA.IDR.Val := pins;
      --  MDER
      --  MDDR
      BBS.embed.GPIO.Due.PIOA.MDDR.Val := pins;
      --  PUDR
      --  PUER
      BBS.embed.GPIO.Due.PIOA.PUER.Val := pins;
      --  ABSR
      BBS.embed.GPIO.Due.PIOA.ABSR.Arr(8) := 0;
      BBS.embed.GPIO.Due.PIOA.ABSR.Arr(9) := 0;
      --  OWER
      --  OWDR
      BBS.embed.GPIO.Due.PIOA.OWDR.Val := pins;
      --
      -- Reset UART receive and transmit
      --
      Serial.CR.RSTRX := 1;
      Serial.CR.RSTTX := 1;
      Serial.CR.RSTSTA := 1;
      --
      --  Right now, for test purposes, just enable the transmitter.
      --
      Serial.CR.TXEN := 1;
      --
      -- Set no parity, channel mode normal (0).
      --
      Serial.MR.PAR := SAM3x8e.UART.No;
      --
      --  The master clock is 84MHz.  Dividing by 16 gives 5.25MHz.  To get a
      --  baud rate of 115200, the clock divisor needs to be 45.57
      --
      Serial.BRGR.CD := SAM3x8e.UART.UART_BRGR_CD_Field(baud_constant);
   end init;
   --
   --  Initialize the specified serial channel.  This will initialize any of
   --  the serial channels specified by the chan_num parameter.
   --
   procedure init(chan_num : port_id; baud_rate : SAM3x8e.UInt32) is
      master_clock  : constant := 84_000_000 / 16;
      baud_constant : constant SAM3x8e.UInt32 := master_clock / baud_rate;
      tx_pin : constant SAM3x8e.UInt32  := 2**Natural(channel(chan_num).tx_pin);
      rx_pin : constant SAM3x8e.UInt32  := 2**Natural(channel(chan_num).rx_pin);
      pins   : constant SAM3x8e.UInt32  := tx_pin or rx_pin;
      ser_io : constant serial_access   := channel(chan_num).port;
   begin
      --
      --  Enable clock for UART
      --
      SAM3x8e.PMC.PMC_Periph.PMC_PCER0.PID.Arr(Integer(channel(chan_num).dev_id)) := 1;
      --
      --  Configure pins to be UART pins.
      --
      --  PER
      --  PDR
      channel(chan_num).pioc.PDR.Val := pins;
      --  OER
      --  ODR
      channel(chan_num).pioc.ODR.Val := pins;
      --  IFER
      --  IFDR
      channel(chan_num).pioc.IFDR.Val := pins;
      --  SODR
      --  CODR
      --  IER
      --  IDR
      channel(chan_num).pioc.IDR.Val := pins;
      --  MDER
      --  MDDR
      channel(chan_num).pioc.MDDR.Val := pins;
      --  PUDR
      --  PUER
      channel(chan_num).pioc.PUER.Val := pins;
      --  ABSR
      channel(chan_num).pioc.ABSR.Arr(Integer(channel(chan_num).tx_pin)) := channel(chan_num).tx_absel;
      channel(chan_num).pioc.ABSR.Arr(Integer(channel(chan_num).rx_pin)) := channel(chan_num).rx_absel;
      --  OWER
      --  OWDR
      channel(chan_num).pioc.OWDR.Val := pins;
      --
      -- Reset UART receive and transmit
      --
      ser_io.CR.RSTRX  := 1;
      ser_io.CR.RSTTX  := 1;
      ser_io.CR.RSTSTA := 1;
      --
      --  Enable the transmitter and receiver
      --
      ser_io.CR.TXEN := 1;
      ser_io.CR.RXEN := 1;
      --
      -- Set no parity, channel mode normal (0).
      --
      ser_io.MR.PAR := SAM3x8e.UART.No;
      --
      --  The master clock is 84MHz.  Dividing by 16 gives 5.25MHz.  To get a
      --  baud rate of 115200, the clock divisor needs to be 45.57
      --
      ser_io.BRGR.CD := SAM3x8e.UART.UART_BRGR_CD_Field(baud_constant);
   end;
   --
   --  Check if the transmitter is ready
   --
   function tx_ready return Boolean is
   begin
      return Serial.SR.TXRDY = 1;
   end;
   --
   function tx_ready(chan : port_id) return Boolean is
   begin
      return channel(chan).port.SR.TXRDY = 1;
   end;
   --
   --  Check if the receiver is ready
   --
   function rx_ready return Boolean is
   begin
      return Serial.SR.RXRDY = 1;
   end;
   --
   function rx_ready(chan : port_id) return Boolean is
   begin
      return channel(chan).port.SR.RXRDY = 1;
   end;
   --
   --  Check if the transmitter is empty.
   --
   function tx_empty return Boolean is
   begin
      return Serial.SR.TXEMPTY = 1;
   end;
   --
   function tx_empty(chan : port_id) return Boolean is
   begin
      return channel(chan).port.SR.TXEMPTY = 1;
   end;

end BBS.embed.due.serial;
