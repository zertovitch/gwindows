------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--          G W I N D O W S . S T A T I C _ C O N T R O L S . W E B         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2008 - 2024 Gautier de Montmollin          --
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

--  This package defines static controls, like labels, enhanced with an
--  interactive feature to access the Internet through the default browser
--  or an e-mail client.
--  For instance, when an URL_Type object is clicked, a browser or a mail
--  window will be opened with the programmed address.

--   4-Jun-2009: GdM: Added Border (as GWindows.Static_Controls)
--  15-Dec-2008: GdM: Created (some code previously in GWin_Util).

package GWindows.Static_Controls.Web is

   --------------------------------------------------------------------------
   --  For URL's there is a "pointing finger" cursor, like in Web browsers.
   --  When the mouse pointer goes over the URL, the cursor becomes a hand
   --  with a pointing finger. If the system (older Windows, or some other
   --  cause) cannot find the standard hand, this URL code tries to find
   --  in the resource a cursor with the name "Finger_cursor".
   --  At bottom, there is a convenient finger.cur to associate with,
   --  uuencoded.
   --------------------------------------------------------------------------

   type URL_Type is new GWindows.Static_Controls.Label_Type with
     record
       URL : GWindows.GString_Unbounded;
     end record;

   --  This mimics GWindows.Static_Controls.Create (Label_Type,...)
   procedure Create
      (Static     : in out URL_Type;
       Parent     : in out GWindows.Base.Base_Window_Type'Class;
       Text       : in     GString;
       URL        : in     GString;  --  Address: local or on the Internet
       Left       : in     Integer;
       Top        : in     Integer;
       Width      : in     Integer;
       Height     : in     Integer;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type    := None;
       ID         : in     Integer        := 0;
       Show       : in     Boolean        := True;
       Is_Dynamic : in     Boolean        := False);

   --  Overriden methods:
   overriding procedure On_Click (Window : in out URL_Type);

   overriding procedure On_Message
     (Window       : in out URL_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

   --  This mimics GWindows.Static_Controls.Create_Label.
   --  URL widget is without variable.
   procedure Create_URL
      (Parent     : in out GWindows.Base.Base_Window_Type'Class;
       Text       : in     GString;
       URL        : in     GString;
       Left       : in     Integer;
       Top        : in     Integer;
       Width      : in     Integer;
       Height     : in     Integer;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type    := None;
       ID         : in     Integer        := 0;
       Show       : in     Boolean        := True);

   --  URL widget is without variable; text and URL are identical.
   procedure Create_URL
      (Parent     : in out GWindows.Base.Base_Window_Type'Class;
       Text_URL   : in     GString;
       Left       : in     Integer;
       Top        : in     Integer;
       Width      : in     Integer;
       Height     : in     Integer;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type    := None;
       ID         : in     Integer        := 0;
       Show       : in     Boolean        := True);

   --  Swap a label against a new URL label, at the same place
   procedure Create_and_Swap
      (To_Show    : in out URL_Type;
       To_Hide    : in out Label_Type;
       Parent     : in out GWindows.Base.Base_Window_Type'Class;
       URL        : in     GString;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type    := None;
       Is_Dynamic : in     Boolean        := False);

end GWindows.Static_Controls.Web;
