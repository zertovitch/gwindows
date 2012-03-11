------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . C O L O R S . A U N I T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------
with AUnit.Assertions;
package body GWindows.Colors.AUnit is

   -----------
   -- Check --
   -----------

   procedure Check (Label     : in String;
                    Computed  : in Color_Range;
                    Expected  : in Color_Range;
                    Tolerance : in Color_Range := 0)
   is begin
      Standard.AUnit.Assertions.Assert
        (abs (Computed - Expected) <= Tolerance,
         Label &
           " got " & Color_Range'Image (Computed) &
           " expecting " & Color_Range'Image (Expected));
   end Check;

   procedure Check
     (Label     : in String;
      Computed  : in Color_Type;
      Expected  : in Color_Type;
      Tolerance : in Color_Range := 0)
   is
      Computed_RGB : constant RGB_Type := To_RGB (Computed);
      Expected_RGB : constant RGB_Type := To_RGB (Expected);
   begin
      Check (Label & ".Red", Computed_RGB.Red, Expected_RGB.Red, Tolerance);
      Check
        (Label & ".Green", Computed_RGB.Green, Expected_RGB.Green, Tolerance);
      Check (Label & ".Blue", Computed_RGB.Blue, Expected_RGB.Blue, Tolerance);
   end Check;

end GWindows.Colors.AUnit;
