------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                          R E A D M E . T X T                             --
--                             Documentation                                --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999-2009 David Botton                    --
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
-- be located on the web at http://www.adapower.com/gnatcom                 --
--                                                                          --
------------------------------------------------------------------------------

Documentation Road Map
======================

GNATCOM documentation comes in a number of different
formats. Documents with the extension texi are the original TexInfo
sources. .HLP files are files in Microsoft help format, .txt are text
based documents, and .html are HTML documents that can loaded in any
browser.

Additional tools needed to build the documentation are located on the
GNATCOM home page at http://www.gnavi.org

The root documentation directory contains the following:

index.html      - HTML based index to GNATCOM documentation
com_with_gnat.* - Interfacing to the Component Object Model with GNAT
gnatcom.*       - GNATCOM Overview and Quick Start Tutorial
gnatcom-ug.*    - GNATCOM User Guide
make.bat        - Batch file to compile doc tutorials and examples
make-doc.bat    - Batch file to create html and help versions of documentation
compile.bat     - Helper batch file for make-doc.bat
clean.bat       - Cleans up this and sub directories of compiled objects
readme.txt      - This document

Sub-directories

client          - Contains the client code for the com_with_gnat documentation
COMObject       - Contains the COMObject code for the com_with_gnat
                  documentation
tutorial        - Source code for the gnatcom overview tutorial
