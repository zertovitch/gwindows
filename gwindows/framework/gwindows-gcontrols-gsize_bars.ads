------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--        G W I N D O W S . G C O N T R O L S . G S I Z E _ B A R S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2023 David Botton                   --
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

with GWindows.Base;
with GWindows.Colors;
with GWindows.Drawing;
with GWindows.Types;
with GWindows.Windows;

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

   procedure Set_Dashes (Window             : in out GSize_Bar_Type;
                         Dash_Height        : in     Natural := 2;
                         Dash_Width         : in     Natural := 2;
                         Spacing_Height     : in     Natural := 0;
                         Spacing_Width      : in     Natural := 5;
                         Number_Of_Dashes_V : in     Natural := 1;
                         Number_Of_Dashes_H : in     Natural := 11);
   --  No dashes are displayed by default.
   --  Use this procedure to define dashes appearance.
   --  For dashes to be displayed, all parameters but spacing must be non zero.
   --  Dashes are vertically and horizontally centered.

   procedure Dashes_Color (Window : in out GSize_Bar_Type;
                           Color  : in     GWindows.Colors.Color_Type);
   --  Set dashes color (flat design)

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

         Dash_Height         : Natural := 0;
         Dash_Width          : Natural := 0;
         Dash_Spacing_Height : Natural := 0;
         Dash_Spacing_Width  : Natural := 0;
         Number_Of_Dashes_V  : Natural := 0;
         Number_Of_Dashes_H  : Natural := 0;
         Dash_Color : GWindows.Colors.Color_Type :=
           GWindows.Colors.System_Color (GWindows.Colors.COLOR_BTNTEXT);
      end record;

end GWindows.GControls.GSize_Bars;
