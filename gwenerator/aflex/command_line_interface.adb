-- Copyright (c) 1990 Regents of the University of California.
-- All rights reserved.
--
-- This software was developed by John Self of the Arcadia project
-- at the University of California, Irvine.
--
-- Redistribution and use in source and binary forms are permitted
-- provided that the above copyright notice and this paragraph are
-- duplicated in all such forms and that any documentation,
-- advertising materials, and other materials related to such
-- distribution and use acknowledge that the software was developed
-- by the University of California, Irvine.  The name of the
-- University may not be used to endorse or promote products derived
-- from this software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
-- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

-- TITLE command line interface
-- AUTHOR: John Self (UCI)
-- DESCRIPTION command line interface body for use with the VERDIX VADS system.
-- NOTES this file is system dependent
-- $Header: /dc/uc/self/tmp/gnat_aflex/src/RCS/command_line_interface.adb,v 1.1 1995/02/06 19:30:01 self Exp self $ 

with Ada.Command_Line; use Ada.Command_Line;

package body COMMAND_LINE_INTERFACE is 
  procedure INITIALIZE_COMMAND_LINE is 
  begin
      for i in 1 .. Ada.Command_Line.Argument_Count
      loop
        ARGV(i):= vstr(Argument(i));
      end loop;
      ARGC := Ada.Command_Line.Argument_Count + 1;
  end INITIALIZE_COMMAND_LINE; 

end COMMAND_LINE_INTERFACE; 
