--  Abstract :
--
--  run test_packing_boxes_aux window
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

with GWindows.Application;
with Test_Packing_Boxes_Aux; use Test_Packing_Boxes_Aux;
procedure Test_Packing_Boxes_Main
is
   Top : Top_Window_Type;
begin
   Create (Top, "Run_packing_boxes");
   Show (Top);
   GWindows.Application.Message_Loop;
end Test_Packing_Boxes_Main;
