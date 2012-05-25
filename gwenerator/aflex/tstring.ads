-- .......................................................................... --
--
-- DISTRIBUTION AND COPYRIGHT:
--                                                           
-- This software is released to the Public Domain (note:
--   software released to the Public Domain is not subject
--   to copyright protection).
-- Restrictions on use or distribution:  NONE
--                                                           
-- DISCLAIMER:
--                                                           
-- This software and its documentation are provided "AS IS" and
-- without any expressed or implied warranties whatsoever.
-- No warranties as to performance, merchantability, or fitness
-- for a particular purpose exist.
--
-- Because of the diversity of conditions and hardware under
-- which this software may be used, no warranty of fitness for
-- a particular purpose is offered.  The user is advised to
-- test the software thoroughly before relying on it.  The user
-- must assume the entire risk and liability of using this
-- software.
--
-- In no event shall any person or organization of people be
-- held responsible for any direct, indirect, consequential
-- or inconsequential damages or lost profits.
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

-- TITLE variable length strings
-- AUTHOR: John Self (UCI)
-- DESCRIPTION these strings are used for many functions
-- $Header: /co/ua/self/arcadia/aflex/ada/src/RCS/tstrings.a,v 1.4 90/01/12 15:20:51 self Exp Locker: self $ 

with VSTRINGS;
pragma Elaborate_All(VSTRINGS);
package TSTRING is 
  new VSTRINGS(1024);  -- This is MAXLINE in misc_defs
