------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . C O M B O _ B O X E S .G E N E R I C _ E N U M E R A L --
--                                                                          --
--                                 B o d y                                  --
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

with GWindows.GStrings;
package body GWindows.Combo_Boxes.Generic_Enumeral is

   CB_GETITEMDATA : constant := 336;
   CB_SETITEMDATA : constant := 337;

   procedure Set_Pos (Combo : in Combo_Box_Type;
                      Index : in Natural;
                      Pos   : in Natural);

   procedure Set_Pos (Combo : in Combo_Box_Type;
                      Index : in Natural;
                      Pos   : in Natural)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_SETITEMDATA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index - 1);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (Pos));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Set_Pos;

   function Get_Pos (Combo : in Combo_Box_Type;
                     Index : in Natural)
                    return Integer;

   function Get_Pos (Combo : in Combo_Box_Type;
                     Index : in Natural)
                    return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Combo);
         uMsg   : Interfaces.C.int      := CB_GETITEMDATA;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (Index - 1);
         lParam : GWindows.Types.Lparam := 0)
      return Integer;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end Get_Pos;

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
      Is_Dynamic : in     Boolean                              := False)
   is
      Index : Natural := 1;
   begin
      GWindows.Combo_Boxes.Create
        (Combo      => Drop_Down_Combo_Box_Type (Combo),
         Parent     => Parent,
         Text       => "",
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         Sort       => Sort,
         ID         => ID,
         Show       => Show,
         Is_Dynamic => Is_Dynamic);

      --  In order to be able to retrieve an enumeral value from a
      --  sorted combo box, we store the 'Pos of the enumeral in the
      --  integer data for the item.
      for I in Enumeral_Type loop
         declare
            Temp : constant GString := GStrings.To_GString_From_String
              (Enumeral_Type'Image (I));
         begin
            Add
              (Combo,
               Temp,
               Index);
            Set_Pos (Combo, Index, Enumeral_Type'Pos (I));
         end;
      end loop;

      Size (Combo, Recommended_Size (Combo));
   end Create;

   function Current
     (Combo : in Combo_Box_Type)
      return Enumeral_Type
   is
      Base_Combo : GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type renames
         GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type (Combo);
   begin
      return Enumeral_Type'Val (Get_Pos (Combo, Current (Base_Combo)));
   end Current;

   procedure Current
     (Combo : in Combo_Box_Type;
      Item  : in Enumeral_Type)
   is
      function Img is new GWindows.GStrings.Enum_Image (Enumeral_Type);
      Index : constant Natural := Find
        (Combo, Img (Item));
   begin
      Current (Combo, Index);
   end Current;

end GWindows.Combo_Boxes.Generic_Enumeral;
