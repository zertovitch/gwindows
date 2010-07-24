------------------------------------------------------------------------------
--                                                                          --
--                   GWINDOWS - Ada 95 RAD GUI Framework                    --
--                                                                          --
--         G W I N D O W S . G C O N T R O L S . D U R A T I O N            --
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

--  Abstract :
--
--  Control for displaying Ada.Calendar.Day_Duration. See children for
--  an editing control, and Date_Time for displaying/editing date and
--  time.
--
--  Design :
--
--  We use Ada.Calendar.Day_Duration, instead of Ada.Duration, because
--  the former has a standard range, allowing a restricted range of
--  formats.
--
--  We visibly derive from GWindows.GControls.GControl_Type, to give the
--  user maximum flexibility in customizing this control's behavior.
--  This means the user can also break things.

with Ada.Calendar;
with GWindows.Drawing;
with GWindows.GControls;
with GWindows.Types;

package GWindows.GControls.Duration is

   type Duration_Type is new GWindows.GControls.GControl_Type with private;
   type Duration_Access is access all Duration_Type;
   type Pointer_To_Duration_Class is access all Duration_Type'Class;

   subtype GDay_Duration_Type is Ada.Calendar.Day_Duration;

   procedure Create
     (Window           : in out Duration_Type;
      Parent           : in out Base.Base_Window_Type'Class;
      Initial_Duration : in     Ada.Calendar.Day_Duration   := 0.0;
      Left             : in     Integer                     := 0;
      Top              : in     Integer                     := 0;
      Width            : in     Integer                     := 0;
      Height           : in     Integer                     := 0;
      Show             : in     Boolean                     := True;
      Is_Dynamic       : in     Boolean                     := False);

   function Recommended_Size (Window : in Duration_Type)
                             return GWindows.Types.Size_Type;
   --  Recommended size, using current font.

   function Text (Window : in Duration_Type) return GString;

   procedure Duration (Window : in out Duration_Type;
                       Value  : in     Ada.Calendar.Day_Duration);

   function Duration (Window : in Duration_Type)
                     return Ada.Calendar.Day_Duration;

private

   type Duration_Type is new GWindows.GControls.GControl_Type with record
      Data           : Ada.Calendar.Day_Duration;
      Formatted_Data : GString (1 .. 5);
   end record;

   procedure On_Create (Window  : in out Duration_Type);

   procedure On_Paint (Window : in out Duration_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);

   ----------
   --  Visible for child package.

   procedure Format_Data (Window : in out Duration_Type);
   --  Set Window.Formatted_Data using current settings.

   function Hours_Minutes (Item : in Ada.Calendar.Day_Duration) return GString;
   --  return time formatted as HH:MM

end GWindows.GControls.Duration;
