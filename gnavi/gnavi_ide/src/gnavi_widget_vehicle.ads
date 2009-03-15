------------------------------------------------------------------------------
--                                                                          --
--         GNAVI - The GWINDOWS Rapid Application Development Tool          --
--                                                                          --
--                G N A V I _ W I D G E T _ V E H I C L E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--              Copyright (C) 1999, 2000, 2001 David Botton                 --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Base;
with GWindows.Windows;
with GWindows.Drawing;
with GWindows.Types;

package GNAVI_Widget_Vehicle is

   Handle_Size : constant := 6;
   Extended_Handle_Size : constant := Handle_Size + 2;
   Minimum_Width : constant := 4;
   Minimum_Height : constant := 4;

   type GNAVI_Widget_Blanket_Type;
   type GNAVI_Widget_Blanket_Access is access all GNAVI_Widget_Blanket_Type;
   --  Forward ref to blanket

   type Handle_Location_Type is
     (Top_Left, Top_Right, Bottom_Left, Bottom_Right,
      Top, Bottom, Left, Right, None);

   type GNAVI_Widget_Vehicle_Type is
     new GWindows.Windows.Window_Type with
      record
         Widget         : GWindows.Base.Pointer_To_Base_Window_Class;
         Blanket        : GNAVI_Widget_Blanket_Access               := null;
         Show_Handles   : Boolean                                   := False;
         Current_Handle : Handle_Location_Type                      := None;
         X_Offset       : Integer;
         Y_Offset       : Integer;
         Size_Bounds    : GWindows.Types.Rectangle_Type;
         Inner_Edit     : Boolean                                   := False;
      end record;
   type GNAVI_Widget_Vehicle_Access is access all GNAVI_Widget_Vehicle_Type;
   --  Vehicle

   procedure Set_Widget
     (Vehicle : in out GNAVI_Widget_Vehicle_Type;
      Widget  : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Set widget in to vehicle

   function Widget_Size (Vehicle : in GNAVI_Widget_Vehicle_Type)
                        return GWindows.Types.Size_Type;
   --  Return the current widget size

   function Widget_Location (Vehicle : in GNAVI_Widget_Vehicle_Type)
                        return GWindows.Types.Point_Type;
   --  Return the current widget location

   procedure On_Character_Down
     (Window      : in out GNAVI_Widget_Vehicle_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GWindows.GCharacter);

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Down
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Mouse_Move
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up
     (Window : in out GNAVI_Widget_Vehicle_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Focus (Window : in out GNAVI_Widget_Vehicle_Type);
   procedure On_Lost_Focus (Window : in out GNAVI_Widget_Vehicle_Type);

   procedure On_Paint
     (Window : in out GNAVI_Widget_Vehicle_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);

   procedure On_Erase_Background
     (Window : in out GNAVI_Widget_Vehicle_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);


   type GNAVI_Widget_Blanket_Type is
     new GWindows.Windows.Window_Type with
      record
         Vehicle     : GNAVI_Widget_Vehicle_Access;
         Track_Mouse : Boolean := False;
         X_Offset    : Integer;
         Y_Offset    : Integer;
      end record;

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Down
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Mouse_Move
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up
     (Window : in out GNAVI_Widget_Blanket_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Erase_Background
     (Window : in out GNAVI_Widget_Blanket_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);

end GNAVI_Widget_Vehicle;
