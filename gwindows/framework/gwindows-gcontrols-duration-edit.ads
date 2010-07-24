------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--      G W I N D O W S . G C O N T R O L S . D U R A T I O N . E D I T     --
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
--  Created By: Stephen Leake

with GWindows.Types;

package GWindows.GControls.Duration.Edit is

   type Duration_Edit_Type is new Duration_Type with private;
   type Duration_Edit_Access is access all Duration_Edit_Type;
   type Pointer_To_Duration_Edit_Class is access all Duration_Edit_Type'Class;

   procedure Create
     (Window           : in out Duration_Edit_Type;
      Parent           : in out GWindows.Base.Base_Window_Type'Class;
      Initial_Duration : in     Ada.Calendar.Day_Duration            := 0.0;
      Left             : in     Integer                              := 0;
      Top              : in     Integer                              := 0;
      Width            : in     Integer                              := 0;
      Height           : in     Integer                              := 0;
      Show             : in     Boolean                              := True;
      Is_Dynamic       : in     Boolean                              := False);

   -----------
   --  Events

   procedure On_Value_Changed_Handler
     (Window  : in out Duration_Edit_Type;
      Handler : in GWindows.Base.Action_Event);

   procedure Fire_On_Value_Changed (Window : in out Duration_Edit_Type);

private

   type Field_Label_Type is (Hour, Minute);

   type Field_Text_Bounds_Type is record
      First : Natural;
      Last  : Natural;
   end record;

   type Field_Array_Text_Bounds_Type is array (Field_Label_Type) of
     Field_Text_Bounds_Type;

   type Field_Array_Rect_Type is array (Field_Label_Type) of
     GWindows.Types.Rectangle_Type;

   type Duration_Edit_Type is new Duration_Type with record
      Current_Field : Field_Label_Type;

      --  Display field index into Formatted_Data. Zero if field not present.
      Field_Text_Bounds : Field_Array_Text_Bounds_Type;

      --  Display field pixel locations, for selecting fields with
      --  mouse clicks. Zero if field not present.
      Field_Rects : Field_Array_Rect_Type;

      --  Events
      On_Value_Changed_Handler : GWindows.Base.Action_Event;
   end record;

   ------------
   --  Override inherited operations

   procedure Format_Data (Window : in out Duration_Edit_Type);
   --  Update field rects.

   procedure On_Character_Down
     (Window      : in out Duration_Edit_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter);

   procedure On_Create (Window : in out Duration_Type);

   procedure On_Focus (Window : in out Duration_Edit_Type);

   procedure On_Lost_Focus (Window : in out Duration_Edit_Type);

   procedure On_Left_Mouse_Button_Down
     (Window : in out Duration_Edit_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Paint
     (Window : in out Duration_Edit_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);

end GWindows.GControls.Duration.Edit;
