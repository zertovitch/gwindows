------------------------------------------------------------------------------
--                                                                          --
--           GWINDOWS - Ada 95 Framework for Windows Development            --
--                                                                          --
--                       G W I N D O W S . E R R O R S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2020 David Botton                   --
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
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

--  Error Handling

with System;

package GWindows.Errors is

   procedure Error_Check (Result : in Integer);
   --  Checks result for a Win32 failure code and if so raises Win32_Error

   function Get_Last_Error return Integer;
   --  Returns the last error thrown by a Win32 API call

   function Get_Last_Error return GString;
   --  Returns the last error as a string identified from the OS

   function Error_To_String (Error_Number : in Integer) return GString;
   --  Asks the operating system to identify the Error_Number

   function To_String (Address : System.Address) return GString;
   --  Converts and address to a string

   Win32_Error : exception;
   --  Raised when a Win32 API fails

end GWindows.Errors;
