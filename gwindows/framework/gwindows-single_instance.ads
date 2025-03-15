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
