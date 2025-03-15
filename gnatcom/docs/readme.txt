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
