with SAM3x8e;
package BBS.embed.AIN.due is
   --
   --  Note that enabling an analog input will superceed any other use for that
   --  pin.  The architecture has defined 16 analog inputs, 0-15, with input 15
   --  being used to measure the CPU temperature.
   --
   --  Mapping between SAM3X8E analog channels and Arduino analog numbers
   --
   --  SAM3X8E   Arduino
   --  -------   -------
   --  AD0       AD07
   --  AD1/WKUP1 AD06
   --  AD2       AD05
   --  AD3       AD04
   --  AD4       AD03
   --  AD5       AD02
   --  AD6       AD01
   --  AD7       AD00
   --  AD8       <unused>
   --  AD9       <unused>
   --  AD10      AD08
   --  AD11      AD09
   --  AD12      AD10
   --  AD13      AD11/TXD3
   --  AD14      <unused>
   --  AD15      CPU temp sensor
   --
   --  Do not use channels marked as unused.  It may cause problems.  Using AD15.
   --  the CPU temperature sensor also seems to cause trouble with tasking.
   --
   --  Define a subtype for the Arduino analogs inputs.  These are mapped to the
   --  analog channels via an array defined in the private section of this
   --  package.
   --
   subtype AIN_Num is Integer range 0 .. 11;
   --
   --
   --  Definitions for object oriented pins
   --
   type Due_AIN_record is new AIN_record with
      record
         channel  : AIN_Num;
      end record;
   --
   --  Enable or disable a specified analog input channel
   --
   procedure enable_ain(self : Due_AIN_record; b : Boolean);
   --
   --  Read an ADC value from a channel
   --
   overriding
   function get(self : Due_AIN_record) return uint12;
   --
   --  Setup the analog to digital controller
   --
   procedure setup_ain;
   --
   --  Enable or disable a specified analog input channel
   --
   procedure enable_ain(c : AIN_Num; b : Boolean);
   --
   --  Start conversion
   --
   procedure start;
   --
   --  Set free running conversion.
   --
   procedure free_run(b : Boolean);
   --
   --  Read an ADC value from a channel
   --
   function get(c : AIN_Num) return UInt12;
private
   --
   --  The processor defines 16 analog input channels.
   --
   subtype AIN_type is Integer range 0 .. 15;
   cpu_temp : constant AIN_type := 15;
   --
   --  Map the Arduino pins to the internal analog input channels
   --
   AIN_map : constant array (AIN_Num'Range) of AIN_type := (7, 6, 5, 4, 3, 2,
                                                            1, 0, 10, 11, 12,
                                                            13);
end BBS.embed.AIN.due;
