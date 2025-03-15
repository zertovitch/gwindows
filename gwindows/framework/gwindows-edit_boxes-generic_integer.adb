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
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
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
