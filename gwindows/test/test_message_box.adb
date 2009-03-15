--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with GWindows.Message_Boxes;
package body Test_Message_Box is

   procedure Test_Ok_Cancel_Box (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Ok_Cancel_Box (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use GWindows.Message_Boxes;

      Result : Message_Box_Result;

   begin
      --  This is an example of a semi-automated test; the user is
      --  instructed what to do, and we verify the results.
      Result := Message_Box
        (Title    => "Test_Ok_Cancel",
         Text     => "Please hit Ok",
         Style    => OK_Cancel_Box,
         Top_Most => True);

      AUnit.Assertions.Assert (Result = OK, "did not get Ok");

      Result := Message_Box
        ("Test_Ok_Cancel",
         "Please hit Cancel",
         OK_Cancel_Box,
         Top_Most => True);
      AUnit.Assertions.Assert (Result = Cancel, "did not get Cancel");
   end Test_Ok_Cancel_Box;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Message_Box");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Ok_Cancel_Box'Access, "Test_Ok_Cancel_Box");
   end Register_Tests;

end Test_Message_Box;
