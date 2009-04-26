------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . P A C K I N G _ B O X E S               --
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

with GWindows.Base;
with GWindows.Windows;
with GWindows.Types;

package GWindows.Packing_Boxes is

   -------------------------------------------------------------------------
   --  Packing_Box_Type
   -------------------------------------------------------------------------

   type Packing_Box_Type is new GWindows.Windows.Window_Type with private;
   type Packing_Box_Access is access all Packing_Box_Type;
   type Pointer_To_Packing_Box_Class is access all Packing_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Packing_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   type Packing_Direction_Type is
     (Horizontal,
      Vertical,
      Horizontal_From_Center,
      Vertical_From_Center,
      Horizontal_From_Right,
      Vertical_From_Bottom);

   procedure Create
     (Window     : in out Packing_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Direction  : in     Packing_Direction_Type               := Horizontal;
      All_Keys   : in     Boolean                              := False;
      Container  : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Width and Height default to 0 to allow user to use
   --  Recommended_Size, Content_Bounds, or Auto_Size_To_Contents after all
   --  children are added.

   -------------------------------------------------------------------------
   --  Packing_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Packing_Direction (Window    : in out Packing_Box_Type;
                                Direction : in     Packing_Direction_Type);
   function Packing_Direction (Window : in Packing_Box_Type)
                              return Packing_Direction_Type;
   --  Direction to pack controls

   procedure Padding (Window : in out Packing_Box_Type;
                      Amount : in     Natural);
   function Padding (Window : in Packing_Box_Type) return Natural;
   --  Set the amount of space in between controls

   procedure Insets (Window : in out Packing_Box_Type;
                     Rect   : in     GWindows.Types.Rectangle_Type);
   function Insets (Window : in Packing_Box_Type)
                   return GWindows.Types.Rectangle_Type;
   --  Set the amount of space around controls

   procedure Inset_Left (Window : in out Packing_Box_Type;
                         Value  : in     Natural);
   procedure Inset_Top (Window : in out Packing_Box_Type;
                        Value  : in     Natural);
   procedure Inset_Right (Window : in out Packing_Box_Type;
                          Value  : in     Natural);
   procedure Inset_Bottom (Window : in out Packing_Box_Type;
                           Value  : in     Natural);
   function Inset_Left (Window : in Packing_Box_Type)
                       return Natural;
   function Inset_Top (Window : in Packing_Box_Type)
                      return Natural;
   function Inset_Right (Window : in Packing_Box_Type)
                        return Natural;
   function Inset_Bottom (Window : in Packing_Box_Type)
                         return Natural;
   --  The amount of space around controls

   procedure Fill_Width (Window : in out Packing_Box_Type;
                         State  : in     Boolean          := True);
   function Fill_Width (Window : in Packing_Box_Type) return Boolean;
   --  Fill controls to width of Packing_Box

   procedure Fill_Height (Window : in out Packing_Box_Type;
                          State  : in     Boolean          := True);
   function Fill_Height (Window : in Packing_Box_Type) return Boolean;
   --  Fill controls to height of Packing_Box

   procedure Auto_Size_To_Contents (Window : in out Packing_Box_Type;
                                    Value  : in     Boolean          := True);
   function Auto_Size_To_Contents (Window : in Packing_Box_Type)
                                  return Boolean;
   --  Automaticly resize Packing_Box to accomidate its contents, reducing or
   --  increasing the size of the packing box as needed.
   --  By default this is False.

   function Recommended_Size (Window : in Packing_Box_Type)
                             return GWindows.Types.Size_Type;
   --  Returns the minimum Recommended_Size. If Fill_Height is True
   --  and Direction = Vertical, returns number of children * max
   --  child recommended height. Similarly for Fill_Width and
   --  Direction = Horizontal.

   function Content_Bounds (Window : in Packing_Box_Type)
                           return GWindows.Types.Size_Type;
   --  Returns the bounds of the contents

   -------------------------------------------------------------------------
   --  Packing_Box_Type - Methods
   -------------------------------------------------------------------------

   procedure Pack (Window : in out Packing_Box_Type);
   --  Pack controls in window

   -------------------------------------------------------------------------
   --  Packing_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Size (Window : in out Packing_Box_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Repack on size change

   procedure Run_Mode (Window : in out Packing_Box_Type;
                       Value  : in     GWindows.Base.Run_Mode_Type);
   --  Overide to catch development mode changes

private
   type Packing_Box_Type is new GWindows.Windows.Window_Type with
      record
         Direction             : Packing_Direction_Type        := Horizontal;
         Padding               : Natural                       := 0;
         Insets                : GWindows.Types.Rectangle_Type := (0, 0, 0, 0);
         Fill_Width            : Boolean                       := False;
         Fill_Height           : Boolean                       := False;
         Auto_Size_To_Contents : Boolean                       := False;
      end record;

end GWindows.Packing_Boxes;
