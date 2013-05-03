------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--        G W I N D O W S . G C O N T R O L S . G S I Z E _ B A R S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2013 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  Size bars can be docked on to a window or panel. When they are dragged
--  they reduce or increase the width or height of their parent control
--  or window. The default implementation of On_Bar_Moved then fires the
--  On_Size event of the grand parent of the control to cause it to dock/pack
--  ts children if it is in the GWindows.Windows.Window_Type'Class.
--  This makes it possible to easily create splitter or tray windows and panes
--  using docking or packing.
--
--  The controls inherited font property (default to parents font property)
--  is used if control text is specified. If the font is not a true type font
--  Arial is used with the same size as the font.

with GWindows.GControls;
with GWindows.Windows;
with GWindows.Base;
with GWindows.Colors;
with GWindows.Drawing;
with GWindows.Types;

package GWindows.GControls.GSize_Bars is

   -------------------------------------------------------------------------
   --  GSize_Bar_Type
   -------------------------------------------------------------------------

   type GSize_Bar_Type is new GWindows.GControls.GControl_Type with private;
   type GSize_Bar_Access is access all GSize_Bar_Type;
   type Pointer_To_GSize_Bar_Class is access all GSize_Bar_Type'Class;

   -------------------------------------------------------------------------
   --  GSize_Bar_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out GSize_Bar_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Location   : in     GWindows.Base.Dock_Type;
      Text       : in     GString                              := "";
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 3;
      Height     : in     Integer                              := 3;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create size bar control

   -------------------------------------------------------------------------
   --  GSize_Bar_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Bar_Moved_Handler (Window  : in out GSize_Bar_Type;
                                   Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Bar_Moved (Window : in out GSize_Bar_Type);

   -------------------------------------------------------------------------
   --  GSize_Bar_Type - Events
   -------------------------------------------------------------------------

   procedure On_Bar_Moved (Window : in out GSize_Bar_Type);

   -------------------------------------------------------------------------
   --  GSize_Bar_Type - Properties
   -------------------------------------------------------------------------

   procedure Maximum_Size (Window : in out GSize_Bar_Type; Value : in Natural);
   function Maximum_Size (Window : in GSize_Bar_Type) return Natural;
   --  Maximum size (width or height) to extend parent window to.

   procedure Minimum_Size (Window : in out GSize_Bar_Type; Value : in Natural);
   function Minimum_Size (Window : in GSize_Bar_Type) return Natural;
   --  Maximum size (width or height) to extend parent window to, by default
   --  size of bar

   procedure Live_Resize (Window : in out GSize_Bar_Type;
                          Value  : in Boolean := True);
   function Live_Resize (Window : in GSize_Bar_Type) return Boolean;
   --  Use live resize or move bar resize, live resize by default

   procedure Move_Bar_Size (Window : in out GSize_Bar_Type;
                            Value  : in     Natural);
   function Move_Bar_Size (Window : in GSize_Bar_Type) return Natural;
   --  Size of Move Bar, used when not using live resize.

   procedure Move_Bar_Color (Window : in out GSize_Bar_Type;
                             Value  : in GWindows.Colors.Color_Type);
   function Move_Bar_Color (Window : in GSize_Bar_Type)
                          return GWindows.Colors.Color_Type;
   --  Size of Move Bar, used when not using live resize.

   procedure Text_Color (Window : in out GSize_Bar_Type;
                         Color  : in     GWindows.Colors.Color_Type);
   function Text_Color (Window : in GSize_Bar_Type)
                       return GWindows.Colors.Color_Type;
   --  The default text color is black

   -------------------------------------------------------------------------
   --  GSize_Bar_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Paint (Window : in out GSize_Bar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);

   procedure On_Left_Mouse_Button_Down
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Mouse_Move (Window : in out GSize_Bar_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GSize_Bar_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

private

   type Move_Bar_Type is new GWindows.GControls.GControl_Type with
      record
         GSize_Bar_Parent : GSize_Bar_Access;
      end record;

   type Move_Bar_Access is access all Move_Bar_Type;
   type Pointer_To_Move_Bar_Class is access all Move_Bar_Type'Class;

   procedure On_Paint (Window : in out Move_Bar_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);

   type GSize_Bar_Type is new GWindows.GControls.GControl_Type with
      record
         In_Size      : Boolean                          := False;
         Minimum      : Natural                          := 3;
         Maximum      : Natural                          := 9999;
         Max_Set      : Boolean                          := False;
         Last_Size    : Natural                          := 3;
         Live_Resize  : Boolean                          := True;
         Bar          : Move_Bar_Access                  := null;
         Bar_Size     : Natural                          := 3;
         Bar_Color    : GWindows.Colors.Color_Type       := 0;
         Text_Color   : GWindows.Colors.Color_Type       := 0;
         On_Bar_Moved_Event : GWindows.Base.Action_Event := null;
      end record;

end GWindows.GControls.GSize_Bars;
