--  Abstract :
--
--  Top level window for testing Static_Controls controls.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

with GWindows.Static_Controls;
with GWindows.Packing_Boxes;
package Test_Static_Controls_Aux is

   type Top_Window_Type is new GWindows.Packing_Boxes.Packing_Box_Type with
   record
      Label : GWindows.Static_Controls.Pointer_To_Label_Class;
   end record;

   procedure On_Create (Window : in out Top_Window_Type);

end Test_Static_Controls_Aux;
