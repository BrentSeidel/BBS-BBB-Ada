--
--  This package contains operations for the TB6612 dual H-Bridge driver.  This
--  can be used to drive a single stepper motor or two DC motors.
--
package BBS.embed.gpio.tb6612 is
   --
   -- Stuff for object oriented interface.  A non-object oriented interface
   -- is not provided for this device.  If you need one, it should be fairly
   -- easy to write one.
   --
   type TB6612_record is tagged private;
   type TB6612_ptr is access TB6612_record;

   --
   --  Initialize the TB6612 device record.
   --
   procedure init(self : in out TB6612_record; pin_a : BBS.embed.GPIO.GPIO;
                  pin_b : BBS.embed.GPIO.GPIO; pin_c : BBS.embed.GPIO.GPIO;
                  pin_d : BBS.embed.GPIO.GPIO);
   --
   --  Set the delay time in mS between steps.  This can be set to 0 if the
   --  delay is being handled elsewhere.
   --
   procedure set_delay(self : in out TB6612_record; wait_time : Natural);
   --
   --  Step a stepper motor the specified number of steps.
   --
   procedure step(self : in out TB6612_record; steps : Integer);
   --
   --  De-energize the coils for a stepper motor.
   --
   procedure stepper_off(self : in out TB6612_record);
   --
   --  For controlling DC motors, each H-Bridge can be controlled separately.
   --  A positive value sets the polarity in one direction, a negative value
   --  reverses the polarity, and a zero value turns it off.
   --
   procedure set_bridge_a(self : in out TB6612_record; value : Integer);
   procedure set_bridge_b(self : in out TB6612_record; value : Integer);

private
   type TB6612_record is tagged record
      initialized : Boolean := False;
      pin_a : BBS.embed.GPIO.GPIO;
      pin_b : BBS.embed.GPIO.GPIO;
      pin_c : BBS.embed.GPIO.GPIO;
      pin_d : BBS.embed.GPIO.GPIO;
      phase : Integer;
      time  : Standard.Duration := 0.005;
   end record;
   --
   --  Phase sequence for stepper motor.
   --
   step_phase : constant array (1 .. 8, 1 .. 4) of BBS.embed.bit :=
     ((1, 0, 1, 0),  --  1. +A +B
      (1, 0, 0, 0),  --  2. +A 0B
      (1, 0, 0, 1),  --  3. +A -B
      (0, 0, 0, 1),  --  4. 0A -B
      (0, 1, 0, 1),  --  5. -A -B
      (0, 1, 0, 0),  --  6. -A 0B
      (0, 1, 1, 0),  --  7. -A +B
      (0, 0, 1, 0)); --  8. 0A +B

end;
