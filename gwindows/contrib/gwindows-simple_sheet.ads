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
--                               2011 Gautier de Montmollin                 --
--                                                                          --
-- This package defines a simple spreadsheet widget, Simple_Sheet_Type.     --
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
-- More information about GWindows and the most current public version can  --
-- be located on the web at http://www.adapower.com/GWindows                --
--                                                                          --
------------------------------------------------------------------------------


-- Change log
--
-- 12-May-2011 GdM: - Set Read_only option to Set_Cell_Text
-- 11-May-2011 GdM: - Simple_Sheet_Type first in Create,
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
  -- A Simple_Sheet_Type object is designed to allow a grid of widgets
  -- to be managed.
  --
  type Simple_Sheet_Type is new GWindows.Scroll_Panels.Scroll_Panel_Type with private;
  type Pointer_To_Simple_Sheet_Class is access all Simple_Sheet_Type'Class;


  procedure Create (
        Sheet          : in out Simple_Sheet_Type;
        Parent         : in out GWindows.Base.Base_Window_Type'Class; -- GdM 2011 fix: parent was first !
        Left           : in     Integer                              := GWindows.Constants.Use_Default;
        Top            : in     Integer                              := GWindows.Constants.Use_Default;
        Width          : in     Integer                              := GWindows.Constants.Use_Default;
        Height         : in     Integer                              := GWindows.Constants.Use_Default;
        Cclass         : in     Gstring                              := "";
        Rows           : in     Positive;
        Columns        : in     Positive;
        Column_Borders : in     Boolean                              := False;
        Row_Borders    : in     Boolean                              := True                            );


   procedure On_Destroy (Sheet : in out Simple_Sheet_Type);

  --
  -- Get_Cell is useful since it returns the base window
  -- that contains the Cell at the given Row and Column
  -- within the Sheet. This allows the user to create a
  -- child within the cell. For any non-trivial use of
  -- the simple sheet, this is the typical way one would
  -- put a widget in the cell.
  --
  function Get_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Base.Pointer_To_Base_Window_Class;

  --
  -- Set_Cell_Text will create an editbox widget within the
  -- cell and store the given text there. Note that for
  -- non trivial operations on the editbox, one can
  -- call Get_Item_Within_Cell to get access to the actual
  -- editbox.
  --
  procedure Set_Cell_Text (
        Sheet     : in out Simple_Sheet_Type;
        Row       : in     Positive;
        Column    : in     Positive;
        Text      : in     GWindows.GString;
        Read_Only : in     Boolean:= False );

  --
  -- Get_Cell_Text returns the text that is currently within
  -- the editbox that is within the cell. If the cell does
  -- not contain and edit box then .....
  --
  function Get_Cell_Text (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Gstring;


  --
  -- Returns the child widget that lives within the cell
  --
  function Get_Item_Within_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Base.Pointer_To_Base_Window_Class;

  --
  -- Sets the width of the given column
  --
  procedure Set_Column_Width (
        Sheet        : in out Simple_Sheet_Type;
        Column_Width : in     Integer            );

  --
  -- Sets the height of the given row
  --
  procedure Set_Row_Height (
        Sheet        : in out Simple_Sheet_Type;
        Row_Height   : in     Integer            );


 private

  -- + GdM
  -- Added Cell_type to have a track of the edit boxes
  -- in addition to cell's panel

  type Cell_type is  record
    panel: aliased GWindows.Panels.Panel_Type;
    edit : GWindows.Edit_Boxes.Edit_Box_Type;
    edit_created: Boolean:= False;
  end record;

  --
  -- We will create the sheet by having rows which are made up of panels.
  --
  type Rows_In_The_Column_Type is array (Natural range <>) of aliased Cell_type ;
  type Access_Column_Type is access Rows_In_The_Column_Type;

  type Column_Type is
     record
        Column_Window  : aliased GWindows.Panels.Panel_Type;
        Rows_In_Column : Access_Column_Type;
     end record;


  type Sheet_Array_Type is array (Natural range <>) of Column_Type;
  type Access_Sheet_Array_Type is access Sheet_Array_Type;

  type Simple_Sheet_Type is new GWindows.Scroll_Panels.Scroll_Panel_Type with
     record
        Sheet_Array : Access_Sheet_Array_Type;
     end record;


end GWindows.Simple_Sheet;
