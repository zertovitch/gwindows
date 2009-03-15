------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                             G W I N D O W S                              --
--                                                                          --
--                               R E A D M E                                --
--                                                                          --
--                  Copyright (C) 1999 - 2005 David Botton                  --
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


Abstract
========

GWindows is a full Win32 RAD GUI Framework with ADO database and
Active X support for Ada 95.

Status
======

GWindows is sucessfuly being used by individuals and companies
world wide for the development of professional Win32 products.

Please joint the e-mail list gnavi-list@gnavi.org if you are experiencing
any problems or have suggestions for improvements.

Documentation
=============

In the doc directory there is  a user_guide in Word format
(html available on the web) and a quick reference text.

Installation
============

For all platforms:

Install GNATCOM version 1.4 or greater.

GNATCOM\Tools should be on the path and when building GBManager from
the GWindows\Tools directory GNATCOM and GWindows should have the
same parent directory. For example C:\GNATCOM and C:\GWindows


Windows 2000/NT/XP:

To build the UNICODE version of GWindows (usable and offering better
performance and internationalization on Windows NT and 2000):

make UNICODE=1 install

To build the ANSI version of GWindows (usable on Windows 9X/ME/NT and
2000, but with performance penalties on NT and 2000):


Windows 9X/ME:

Windows 9X/ME only supports ANSI APIs. Install using:

make ANSI=1 install


What's New in GWindows 1.3
==========================

See changes.txt
