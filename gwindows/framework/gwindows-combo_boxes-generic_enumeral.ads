------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . C O M B O _ B O X E S .G E N E R I C _ E N U M E R A L --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Enumeral_Type is (<>);
package GWindows.Combo_Boxes.Generic_Enumeral is

   -------------------------------------------------------------------------
   --  Combo_Box_Type
   -------------------------------------------------------------------------

   type Combo_Box_Type is new GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type
     with null record;
   type Combo_Box_Access is access all Combo_Box_Type;
   type Pointer_To_Combo_Box_Class is access all Combo_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Combo_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Combo      : in out Combo_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Combo Box

   ------------
   --  New Combo_Box_Type operations

   function Current (Combo : in Combo_Box_Type) return Enumeral_Type;

   procedure Current
     (Combo : in Combo_Box_Type;
      Item  : in Enumeral_Type);

end GWindows.Combo_Boxes.Generic_Enumeral;
