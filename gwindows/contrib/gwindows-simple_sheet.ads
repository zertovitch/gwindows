------------------------------------------------------------------------------
--                                                                          --
--           GWindows - Ada 95+ Framework for Windows Development           --
--                                                                          --
--                G W I N D O W S . S I M P L E _ S H E E T                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 0.1 $                              --
--                                                                          --
--                 Copyright (C) 1999 - 2002 Jeffrey Creem                  --
--                               2011 - 2012 Gautier de Montmollin          --
--                                                                          --
-- This package defines a simple spreadsheet widget, Simple_Sheet_Type.     --
-- Currently it looks more like a data grid in Access (a grid of            --
-- Edit_Box'es) than an Excel sheet                                         --
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
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  Change log
--
--   5-Jul-2012 GdM: - Fixed Set_Cell_Text when cell alread existing
--                   - Added Copy_to_Clipboard and Paste_from_Clipboard
--  12-May-2011 GdM: - Set Read_only option to Set_Cell_Text
--  11-May-2011 GdM: - Simple_Sheet_Type first in Create,
--                      allows for writing in Ada 2005 style
--                      s.Create(parent,...)
--                  - Fixes
--                  - Added destructor
--                  - Get_text takes text from edit box

with GWindows.Constants;
with GWindows.Scroll_Panels;
with GWindows.Panels;
with GWindows.Edit_Boxes;
with GWindows.Base;

package GWindows.Simple_Sheet is

  --
  --  A Simple_Sheet_Type object is designed to allow a grid of widgets
  --  to be managed.
  --
  type Simple_Sheet_Type is
    new GWindows.Scroll_Panels.Scroll_Panel_Type with private;

  type Pointer_To_Simple_Sheet_Class is access all Simple_Sheet_Type'Class;

  procedure Create (
        Sheet          : in out Simple_Sheet_Type;
        Parent         : in out GWindows.Base.Base_Window_Type'Class;
        --  GdM 2011 fix: parent was first !
        Left           : in     Integer    := GWindows.Constants.Use_Default;
        Top            : in     Integer    := GWindows.Constants.Use_Default;
        Width          : in     Integer    := GWindows.Constants.Use_Default;
        Height         : in     Integer    := GWindows.Constants.Use_Default;
        Cclass         : in     GString    := "";
        Rows           : in     Positive;
        Columns        : in     Positive;
        Column_Borders : in     Boolean    := False;
        Row_Borders    : in     Boolean    := True
  );

   procedure On_Destroy (Sheet : in out Simple_Sheet_Type);

  --
  --  Get_Cell is useful since it returns the base window
  --  that contains the Cell at the given Row and Column
  --  within the Sheet. This allows the user to create a
  --  child within the cell. For any non-trivial use of
  --  the simple sheet, this is the typical way one would
  --  put a widget in the cell.
  --
  function Get_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive)
    return GWindows.Base.Pointer_To_Base_Window_Class;

  --
  --  Set_Cell_Text will create an editbox widget within the
  --  cell and store the given text there. Note that for
  --  non trivial operations on the editbox, one can
  --  call Get_Item_Within_Cell to get access to the actual
  --  editbox.
  --
  procedure Set_Cell_Text (
        Sheet     : in out Simple_Sheet_Type;
        Row       : in     Positive;
        Column    : in     Positive;
        Text      : in     GWindows.GString;
        Read_Only : in     Boolean := False);

  --
  --  Get_Cell_Text returns the text that is currently within
  --  the editbox that is within the cell. If the cell does
  --  not contain and edit box then .....
  --
  function Get_Cell_Text (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive)
    return GWindows.GString;

  --
  --  Returns the child widget that lives within the cell
  --
  function Get_Item_Within_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive)
    return GWindows.Base.Pointer_To_Base_Window_Class;

  --
  --  Sets the width of the given column
  --
  procedure Set_Column_Width (
        Sheet        : in out Simple_Sheet_Type;
        Column_Width : in     Integer);

  --
  --  Sets the height of the given row
  --
  procedure Set_Row_Height (
        Sheet        : in out Simple_Sheet_Type;
        Row_Height   : in     Integer);

  --
  --  Copy entire sheet to the Clipboard
  --
  procedure Copy_to_Clipboard (Sheet : in Simple_Sheet_Type);

  --
  --  Copy a portion of a sheet to the Clipboard
  --
  procedure Copy_to_Clipboard (
    Sheet        : in Simple_Sheet_Type;
    Row_Start    : in Positive;
    Row_End      : in Positive;
    Column_Start : in Positive;
    Column_End   : in Positive
  );

  Too_many_columns, Too_many_rows : exception;

  --
  --  Paste from the Clipboard.
  --
  procedure Paste_from_Clipboard (
        Sheet                 : in out Simple_Sheet_Type;
        Row_Start             : in     Positive := 1;
        Column_Start          : in     Positive := 1;
        Complain_if_too_large : in     Boolean := False);

private

  --  + GdM
  --  Added Cell_type to have a track of the edit boxes
  --  in addition to cell's panel

  type Cell_type is  record
    panel : aliased GWindows.Panels.Panel_Type;
    edit : GWindows.Edit_Boxes.Edit_Box_Type;
    edit_created : Boolean := False;
  end record;

  --
  --  We will create the sheet by having rows which are made up of panels.
  --
  type Rows_In_The_Column_Type is
     array (Positive range <>) of aliased Cell_type;
  type Access_Column_Type is access Rows_In_The_Column_Type;

  type Column_Type is
     record
        Column_Window  : aliased GWindows.Panels.Panel_Type;
        Rows_In_Column : Access_Column_Type;
     end record;

  type Sheet_Array_Type is
     array (Positive range <>) of Column_Type;
  type Access_Sheet_Array_Type is access Sheet_Array_Type;

  type Simple_Sheet_Type is new GWindows.Scroll_Panels.Scroll_Panel_Type with
     record
        Rows        : Positive;
        Columns     : Positive;
        Sheet_Array : Access_Sheet_Array_Type;
     end record;

   --  The number of rows and columns are stored elsewhere (in the array
   --  ranges), but in a way more difficult to access, especially since
   --  the sheet is stored column-wise.

end GWindows.Simple_Sheet;
