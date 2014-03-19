------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                           R E A D M E . T X T                            --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- More information about GNATCOM and the most current public version can   --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gnatcom                                           --
--   http://www.adapower.com/gnatcom                                        --
--                                                                          --
------------------------------------------------------------------------------

GNATCOM is a collection of packages for use in interfacing Ada with
Type Libraries, COM+/COM/DCOM Objects, ActiveX, Automation and OLE.


Installation
============

All you have is to run the setup program (e.g. GWindows Setup 16-Feb-2012.exe
downloaded from here http://sf.net/projects/gnavi/files/latest/download ) which
unpacks a copy of the framework with samples, tutorials and other goodies.
Alternatively you can retrieve a copy of the SVN repository from
http://sf.net/p/gnavi/code/ .
So normally, if you read this readme.txt document, most of the installation
already happened!

By running build_tools.cmd you will obtain executables for the GNATCOM tools.
For ease of use, it may be advisable to place the tools directory on the path
or to copy the executables into a directory on the path.


Documentation
=============

Documentation for GNATCOM currently consists of general instructions
on thin binding to COM with GNAT and an overview and tutorial of
GNATCOM.

To create html and Windows Help versions of the documentation run:

   make docs

in the root GNATCOM directory. To create these files, makeinfo is needed.


Copyright and Licensing Considerations
======================================

The GNATCOM framework is included in full source form. It is licensed
using a modified version of the General Public License (GPL) which
allows unlimited distribution without license fees, and incorporation
into any program executable without restriction. In particular, the
GNATCOM framework can be incorporated into proprietary or classified
programs, and in no way restricts the desired licensing or
distribution of such programs.

The GNATCOM tools are covered by the GPL, which means that they are
freely redistributable, and that source code is available. You may
freely modify the tools for your own use, but if you redistribute
modified versions of the GNATCOM tools, then they must themselves be
distributed under the terms of the GPL, and in particular you cannot
incorporate the GNATCOM tools into proprietary programs.

For more information on the licensing provisions, consult the headers
of the appropriate source files.


Submitting Bug Reports
======================

Please see http://www.gnavi.org
