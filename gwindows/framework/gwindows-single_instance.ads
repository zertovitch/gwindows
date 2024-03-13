------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--              G W I N D O W S . S I N G L E _ I N S T A N C E             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2024 Nicolas Pinault                       --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------
--
--  Manage a single instance of an application.
--

generic
  with procedure Process_Argument (Position, Total : Positive; Arg : String);
  --  Command-line arguments will be sent by Manage_Single_Instance
  --  from a new, short-lived, instance of the application, to an already
  --  running instance, which will receive them from Windows
  --  and pass them further to Process_Argument.
  --
  --  When the instance is the first to be running,
  --  you can process the command-line arguments directly:
  --
  --      for i in 1 .. Argument_Count loop
  --        App_Process_Argument (i, Argument_Count, Argument (i));
  --      end loop;

package GWindows.Single_Instance is

  procedure Manage_Single_Instance
    (Application_Class_Name    : in     GString;
     Application_Instance_Name : in     GString;
     Exit_Requested            :    out Boolean);

  --  Exit_Requested =
  --
  --     False : no other instance is running, or another
  --     =====   instance is running, but its main window
  --             cannot be found by the system.
  --
  --     True : another instance is running, was found by
  --     ====   the system and we have passed to it the command-line
  --            arguments of our instance, which can stop safely.

end GWindows.Single_Instance;
