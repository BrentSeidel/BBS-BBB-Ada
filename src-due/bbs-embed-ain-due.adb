--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
with SAM3x8e.ADC;
with SAM3x8e.PMC;
with BBS.embed.due.dev;

package body BBS.embed.AIN.due is
   --
   --  Setup the analog to digital controller
   --
   procedure setup_ain is
   begin
      --
      --  Enable clock for ADC
      --
      SAM3x8e.PMC.PMC_Periph.PMC_PCER1.PID.Arr(BBS.embed.due.dev.ADC_ID) := 1;
      --
      --  Setup some reasonable values for the ADC
      --
      --
      --  Set modes - these are probably defaults.
      --
      SAM3x8e.ADC.ADC_Periph.MR.TRGEN    := SAM3x8e.ADC.Dis;
      SAM3x8e.ADC.ADC_Periph.MR.LOWRES   := SAM3x8e.ADC.Bits_12;
      SAM3x8e.ADC.ADC_Periph.MR.SLEEP    := SAM3x8e.ADC.Sleep;
      SAM3x8e.ADC.ADC_Periph.MR.FWUP     := SAM3x8e.ADC.Off;
      SAM3x8e.ADC.ADC_Periph.MR.FREERUN  := SAM3x8e.ADC.Off;
      SAM3x8e.ADC.ADC_Periph.MR.STARTUP  := SAM3x8e.ADC.Sut0;
      SAM3x8e.ADC.ADC_Periph.MR.SETTLING := SAM3x8e.ADC.Ast3;
      SAM3x8e.ADC.ADC_Periph.MR.ANACH    := SAM3x8e.ADC.None;
      SAM3x8e.ADC.ADC_Periph.MR.USEQ     := SAM3x8e.ADC.Num_Order;
      --
      --  Use a gain of 1.
      --
      for i in AIN_type'Range loop
         SAM3x8e.ADC.ADC_Periph.CGR.Arr(i) := 1;
      end loop;
      --
      --  Turn the temperature sensor on
      --
      SAM3x8e.ADC.ADC_Periph.ACR.TSON := 1;
   end setup_ain;
   --
   --  Enable or disable a specified analog input channel
   --
   procedure enable_ain(c : AIN_Num; b : Boolean) is
      ain : constant AIN_type := AIN_map(c);
   begin
      if b then
         SAM3x8e.ADC.ADC_Periph.CHER.CH.Arr(ain) := 1;
      else
         SAM3x8e.ADC.ADC_Periph.CHDR.CH.Arr(ain) := 1;
      end if;
   end;
   --
   --  Enable or disable a specified analog input channel
   --
   procedure enable_ain(self : Due_AIN_record; b : Boolean) is
   begin
      enable_ain(self.channel, b);
   end;
   --
   --  Start conversion
   --
   procedure start is
   begin
      SAM3x8e.ADC.ADC_Periph.CR.START := 1;
   end;
   --
   --  Set free running conversion.
   --
   procedure free_run(b : Boolean) is
   begin
      if b then
         SAM3x8e.ADC.ADC_Periph.MR.FREERUN := SAM3x8e.ADC.On;
      else
         SAM3x8e.ADC.ADC_Periph.MR.FREERUN := SAM3x8e.ADC.Off;
      end if;
   end;
   --
   --  Read an ADC value from a channel
   --
   function get(c : AIN_Num) return uint12 is
      ain : constant AIN_type := AIN_map(c);
   begin
      return uint12(SAM3x8e.ADC.ADC_Periph.CDR(ain).DATA);
   end;
   --
   --  Read an ADC value from a channel
   --
   overriding
   function get(self : Due_AIN_record) return uint12 is
   begin
      return uint12(SAM3x8e.ADC.ADC_Periph.CDR(AIN_map(self.channel)).DATA);
   end;

end BBS.embed.AIN.due;
