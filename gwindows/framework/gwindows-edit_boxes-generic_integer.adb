------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . E D I T _ B O X E S .G E N E R I C _ I N T E G E R     --
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

with Ada.Strings.Fixed;
with GWindows.GStrings;
with GWindows.Message_Boxes;
package body GWindows.Edit_Boxes.Generic_Integer is

   procedure Create
     (Edit       : in out Edit_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Initial    : in     Integer_Type
      := Integer_Type'First;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      use type Interfaces.C.unsigned;
   begin

      Create_Control
        (Window     => Edit,
         Parent     => Parent,
         Win_Class  => "Edit",
         Text       => "",
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         ID         => ID,
         Styles     => WS_TABSTOP or ES_AUTOHSCROLL or ES_NUMBER,
         Is_Dynamic => Is_Dynamic);

      Border (Edit);

      if Width = 0 or Height = 0 then
         declare
            New_Size : GWindows.Types.Size_Type := Recommended_Size (Edit);
         begin
            if Width /= 0 then
               New_Size.Width := Width;
            end if;
            if Height /= 0 then
               New_Size.Height := Height;
            end if;
            Size (Edit, New_Size);
         end;
      end if;

      if Show then
         GWindows.Edit_Boxes.Generic_Integer.Show (Edit);
      end if;

      Current (Edit, Initial);
   end Create;

   procedure On_Update (Edit : in out Edit_Box_Type)
   is
      use Ada.Strings.Fixed;
   begin
      --  Catch errors before they are displayed.
      declare
         Image : constant String := GWindows.GStrings.To_String (Text (Edit));
      begin
         if Image'Length = 0 then
            --  Assume the user will type something else soon. Can't
            --  set Edit.Value to 0; might not be in Integer_Type.
            null;
         else
            Edit.Value := Integer_Type'Value (Image);
         end if;
      exception
         when Constraint_Error =>
            --  Image outside Integer_Type range. This check is done
            --  even if other constraint checks are suppressed.

            --  FIXME: 'Text' changes the caret position, but the
            --  GWindows.Carets functions don't seem to work here, so
            --  we can't fix it.
            Text
              (Edit,
               GWindows.GStrings.To_GString_From_String
                 (Trim (Integer_Type'Image (Edit.Value), Ada.Strings.Left)));

            GWindows.Message_Boxes.Message_Beep
              (GWindows.Message_Boxes.Hand_Icon);
      end;

      Fire_On_Update (Edit);
   end On_Update;

   function Current
     (Edit : in Edit_Box_Type)
      return Integer_Type
   is
   begin
      return Edit.Value;
   end Current;

   procedure Current
     (Edit : in out Edit_Box_Type;
      Item : in     Integer_Type)
   is
      use Ada.Strings.Fixed;
   begin
      Edit.Value := Item;
      Text (Edit, GWindows.GStrings.To_GString_From_String
              (Trim (Integer_Type'Image (Item), Ada.Strings.Left)));
   end Current;

end GWindows.Edit_Boxes.Generic_Integer;
