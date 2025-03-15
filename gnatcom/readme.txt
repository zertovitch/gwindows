------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                              G N A T C O M                               --
--                                                                          --
--                           R E A D M E . T X T                            --
--                                                                          --
--                 Copyright (C) 1999 - 2023 David Botton                   --
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
-- More information about GNATCOM and the most current public version can   --
-- be located on the web at one of the following places:                    --
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
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

Please check the Web links above.
