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
