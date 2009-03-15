------------------------------------------------------------------------------
--                                                                          --
--                       gwindows.windows.ex_windows                        --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------
--  The Ex_Window_Type supports the tooltip-notification
--  Necessary to display the tooltip-text for a toolbar-button
------------------------------------------------------------------------------

with GWindows.Base; use GWindows.Base;
with GWindows.Common_Controls.Ex_Tb; use GWindows.Common_Controls.Ex_Tb;

package GWindows.Windows.Ex_Windows is

   type Ex_Window_Type is new Window_Type with private;
   type Ex_Window_Access is access all Ex_Window_Type;
   type Pointer_To_Ex_Window_Class is access all Ex_Window_Type'Class;

   procedure Activate_Tooltip (Window  : in out Ex_Window_Type;
                               Toolbar : in out Ex_Toolbar_Control_Type);
   --  Activate the Tooltips

   --------------------
   -- event-handling --
   --------------------

   procedure On_Notify (Window       : in out Ex_Window_Type;
                        Message      : in     Pointer_To_Notification;
                        Control      : in     Pointer_To_Base_Window_Class;
                        Return_Value : in out Interfaces.C.long);
private

   type Ex_Window_Type is new Window_Type with
      record
         Toolbar : Ex_Toolbar_Control_Access;
      end record;

end GWindows.Windows.Ex_Windows;
