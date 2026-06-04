------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                        G W I N D O W S . H T M L                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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

with Ada.Strings.Unbounded;

with GNATCOM.Dispinterface;

with GWindows.ActiveX;
with GWindows.Base;
with GWindows.Types;

with Interfaces.C;

package GWindows.Html is

   type Html_Type is new ActiveX.ActiveX_Type with private;

   type Action_Event is access procedure (Html : Html_Type'Class);

   procedure Create
               (Html          : in out Html_Type;
                Parent        : in out GWindows.Base.Base_Window_Type'Class;
                Left, Top     :        Natural;
                Width, Height :        Natural);

   procedure Create
     (Html          : in out Html_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left, Top     :        Natural;
      Width, Height :        Natural;
      FileName      : in     Ada.Strings.Unbounded.Unbounded_String);

   procedure Accept_File_Drag_And_Drop
     (Html  : Html_Type;
      State : Boolean := True);

   procedure Url (Html : Html_Type;
                  Url  : GString);

   procedure On_Mouse_Activate_Handler (Html    : in out Html_Type;
                                        Handler :        Action_Event);

   procedure GoBack (Html : in out Html_Type);

   function FileName (Html : in Html_Type) return String;

private

   type Html_Type is new ActiveX.ActiveX_Type with
      record
         Browser                   : GNATCOM.Dispinterface.Dispinterface_Type;
         On_Mouse_Activate_Handler : Action_Event;
         FileName                  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure On_Message
     (Html         : in out Html_Type;
      message      : Interfaces.C.unsigned;
      wParam       : GWindows.Types.Wparam;
      lParam       : GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

end GWindows.Html;
