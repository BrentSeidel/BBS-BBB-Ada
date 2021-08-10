with Ada.Real_Time;
use type Ada.Real_Time.Time;
with BBS.embed.log;
package body BBS.embed.gpio.tb6612 is

   --
   --  Initialize the TB6612 device record.  Assign the GPIO pins, set the
   --  delay time to 5mS, which is a reasonable default, and write the phase 1
   --  values to the pins.
   --
   procedure init(self : in out TB6612_record; pin_a : BBS.embed.GPIO.GPIO;
                  pin_b : BBS.embed.GPIO.GPIO; pin_c : BBS.embed.GPIO.GPIO;
                  pin_d : BBS.embed.GPIO.GPIO) is
   begin
      self.pin_a := pin_a;
      self.pin_b := pin_b;
      self.pin_c := pin_c;
      self.pin_d := pin_d;
      self.phase := 1;
      self.initialized := True;
      self.time := 0.005;
      self.pin_a.set(step_phase(1, 1));
      self.pin_b.set(step_phase(1, 2));
      self.pin_c.set(step_phase(1, 3));
      self.pin_d.set(step_phase(1, 4));
   end;
   --
   --  Set the delay time in mS between steps.  This can be set to 0 if the
   --  delay is being handled elsewhere.
   --
   procedure set_delay(self : in out TB6612_record; wait_time : Natural) is
   begin
      self.time := Standard.Duration(wait_time)/1000.0;
   end;
   --
   --  Step a stepper motor the specified number of steps.
   --
   procedure step(self : in out TB6612_record; steps : Integer) is
   begin
      --
      --  If the number of steps is exactly 0, then do nothing.
      --
      if steps > 0 then
         for step in 1 .. steps loop
            if self.phase = 8 then
               self.phase := 1;
            else
               self.phase := self.phase + 1;
            end if;
            self.pin_a.set(step_phase(self.phase, 1));
            self.pin_b.set(step_phase(self.phase, 2));
            self.pin_c.set(step_phase(self.phase, 3));
            self.pin_d.set(step_phase(self.phase, 4));
            delay until Ada.Real_Time.Clock +
              Ada.Real_Time.To_Time_Span(self.time);
         end loop;
      elsif steps < 0 then
         for step in 1 .. -steps loop
            if self.phase = 1 then
               self.phase := 8;
            else
               self.phase := self.phase - 1;
            end if;
            self.pin_a.set(step_phase(self.phase, 1));
            self.pin_b.set(step_phase(self.phase, 2));
            self.pin_c.set(step_phase(self.phase, 3));
            self.pin_d.set(step_phase(self.phase, 4));
            delay until Ada.Real_Time.Clock +
              Ada.Real_Time.To_Time_Span(self.time);
         end loop;
      end if;
   end;
   --
   --  De-energize the coils for a stepper motor.  Setting all the pins to 1
   --  would probably also work.
   --
   procedure stepper_off(self : in out TB6612_record) is
   begin
      self.pin_a.set(0);
      self.pin_b.set(0);
      self.pin_c.set(0);
      self.pin_d.set(0);
   end;
   --
   --  For controlling DC motors, each H-Bridge can be controlled separately.
   --  A positive value sets the polarity in one direction, a negative value
   --  reverses the polarity, and a zero value turns it off.
   --
   procedure set_bridge_a(self : in out TB6612_record; value : Integer) is
   begin
      if value < 0 then
         self.pin_a.set(0);
         self.pin_b.set(1);
      elsif value > 0 then
         self.pin_a.set(1);
         self.pin_b.set(0);
      else
         self.pin_a.set(0);
         self.pin_b.set(0);
      end if;
   end;
   --
   procedure set_bridge_b(self : in out TB6612_record; value : Integer) is
   begin
      if value < 0 then
         self.pin_c.set(0);
         self.pin_d.set(1);
      elsif value > 0 then
         self.pin_c.set(1);
         self.pin_d.set(0);
      else
         self.pin_c.set(0);
         self.pin_d.set(0);
      end if;
   end;
   --
end;

