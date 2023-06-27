------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--               G W I N D O W S . P E R S I S T E N C E _ I O              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2023 Gautier de Montmollin                 --
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

--  Windows_Persistence_IO provides the Input/Output of persistence
--  values that are meant to survive the execution time span of an
--  application. For example, if you use the Write_Key procedure
--  with key = Windows_Left and value of the window's left edge
--  horizontal position when leaving the application, you can restore
--  that position when the application is started again.
--
--  Usually, under MS Windows, this is done in the Windows registry,
--  on a per-user basis. However, in a context where you can't or don't
--  want the registry to be used, it is possible to use a configuration
--  file for that purpose. An empty file will be good to begin with, and
--  will be later replaced by a valid one. The application runs in a
--  so-called "stealth mode" and saves its persistence items in the
--  configuration file each time it is closed, and re-reads it
--  each time it is started.
--  If the configuration file is read-only, it won't be overwritten.
--  If the configuration file disappears, the registry will be used again.
--
--  To summarize, the persistence works in the following two modes:
--
--    my_app.cfg doesn't exist besides my_app.exe ---> normal mode (registry).
--    my_app.cfg exists besides my_app.exe        ---> stealth mode.
--
--  Windows_Persistence_IO is used in the following open-source projects:
--
--    AZip   : https://azip.sourceforge.io/
--    LEA    : https://l-e-a.sourceforge.io/
--
--  Mirrors of those projects are located here: https://github.com/zertovitch

generic
  app_display_name : String;  --  Application name for display & registry
  app_file_name    : String;  --  Application file name, without extension
  app_url          : String;  --  Application's Internet address

  type Persistence_Key is (<>);

package GWindows.Persistence_IO is

  function Read_Key (key : Persistence_Key) return GString;

  procedure Write_Key (key : Persistence_Key; value : GString);

  function Is_Config_File_Available return Boolean;
  --  ^ When True, we are in "stealth mode" and don't want to
  --    leave any trace in the registry!

end GWindows.Persistence_IO;
