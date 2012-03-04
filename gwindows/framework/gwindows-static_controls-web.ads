------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95+ Framework for Windows Development         --
--                                                                          --
--          G W I N D O W S . S T A T I C _ C O N T R O L S . W E B         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2008 - 2011 Gautier de Montmollin          --
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

--  This package defines static controls, like labels, enhanced with an
--  interactive feature to access the Internet through the default browser
--  or an e-mail client.
--  For instance, when an URL_Type object is clicked, a browser or a mail
--  window will be opened with the programmed address.

--  4-Jun-2009: GdM: Added Border (as GWindows.Static_Controls)
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

   --  This mimics GWindows.Static_Controls.Create(Label_Type,...)
   procedure Create
      (Static     : in out URL_Type;
       Parent     : in out GWindows.Base.Base_Window_Type'Class;
       Text       : in     GString;
       URL        : in     GString; -- Address local or on the Internet
       Left       : in     Integer;
       Top        : in     Integer;
       Width      : in     Integer;
       Height     : in     Integer;
       Alignment  : in     Alignment_Type                       :=
         GWindows.Static_Controls.Left;
       Border     : in     Border_Type                          := None;
       ID         : in     Integer                              := 0;
       Show       : in     Boolean                              := True;
       Is_Dynamic : in     Boolean                              := False);

   --  Overriden methods:
   procedure On_Click (Window : in out URL_Type);

   procedure On_Message
     (Window       : in out URL_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

   --  This mimics GWindows.Static_Controls.Create_Label:
   --  Label without variable
   procedure Create_URL
      (Parent     : in out GWindows.Base.Base_Window_Type'Class;
       Text       : in     GString;
       URL        : in     GString; -- Address local or on the Internet
       Left       : in     Integer;
       Top        : in     Integer;
       Width      : in     Integer;
       Height     : in     Integer;
       Alignment  : in     Alignment_Type                       :=
         GWindows.Static_Controls.Left;
       Border     : in     Border_Type                          := None;
       ID         : in     Integer                              := 0;
       Show       : in     Boolean                              := True);

   --  Swap a label against a new URL label, at the same place
   procedure Create_and_Swap
      (To_Show    : in out URL_Type;
       To_Hide    : in out Label_Type;
       Parent     : in out GWindows.Base.Base_Window_Type'Class;
       URL        : in     GString;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type                          := None;
       Is_Dynamic : in     Boolean := False
      );

   --
   --  begin 600 finger.cur
   --  M```"``$`("````8````P`0``%@```"@````@````0`````$``0````````$`
   --  M````````````````````````````____````````````````````````````
   --  M`````````````````````````````````?X```'^```#_P```_\```?_```'
   --  M_X``#_^```O_@``;_X``._^``#/]@``#;8```VT```-L```#8````P````,`
   --  M```#`````P````,`````````____________________________________
   --  M__________________P`___\`/___`#___@`?__X`'__\`!___``/__@`#__
   --  MX``__\``/_^``#__@``__X@`/__X`'__^`#___@#___X'___^'____A____X
   --  +?___^'____S_____
   --  `
   --  end

end GWindows.Static_Controls.Web;
