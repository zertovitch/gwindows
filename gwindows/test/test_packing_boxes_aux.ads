--  Abstract :
--
--  Top level window for testing packing boxes.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with GWindows.Packing_Boxes;
with GWindows.Buttons;
with GWindows.Types;
package Test_Packing_Boxes_Aux is

   type Top_Window_Type is new GWindows.Packing_Boxes.Packing_Box_Type with
   record
      --  FIXME: packing box should own and destroy these.
      --  But they are accessible for Check, below.
      Hbox     : GWindows.Packing_Boxes.Packing_Box_Type;
      Button_1 : GWindows.Buttons.Button_Type;
      Button_2 : GWindows.Buttons.Button_Type;
      Button_3 : GWindows.Buttons.Button_Type;
   end record;

   procedure On_Create (Window : in out Top_Window_Type);

   procedure Check
     (Label              : in String;
      Computed           : in Top_Window_Type;
      Expected_Vbox_Size : in GWindows.Types.Size_Type;
      Expected_Button_1  : in GWindows.Types.Rectangle_Type;
      Expected_Hbox      : in GWindows.Types.Rectangle_Type;
      Expected_Button_2  : in GWindows.Types.Rectangle_Type;
      Expected_Button_3  : in GWindows.Types.Rectangle_Type);
   --  If size and placement of components of Computed (relative to
   --  their respective parents) are not equal to Expected_*, raise
   --  AUnit.Assertions.Assertion_Error, with appropriate message.

end Test_Packing_Boxes_Aux;
