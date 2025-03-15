------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . L I S T _ B O X E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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

with GWindows.Base;
with GWindows.Types;

package GWindows.List_Boxes is

   -------------------------------------------------------------------------
   --  List_Box_Type
   -------------------------------------------------------------------------

   type List_Box_Type is new GWindows.Base.Base_Window_Type with private;
   type List_Box_Access is access all List_Box_Type;
   type Pointer_To_List_Box_Class is access all List_Box_Type'Class;

   -------------------------------------------------------------------------
   --  List_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List       : in out List_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create List Box

   -------------------------------------------------------------------------
   --  List_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Current (List : in List_Box_Type;
                      Item : in Natural);
   --  Set current selected item (1-based index).
   --  0 means: unselect.

   function Current (List : in List_Box_Type) return Natural;
   --  Returns currently selected / focused item.
   --  0 means: no item is selected.

   procedure Item_Data (List : List_Box_Type;
                        Item : Natural;
                        Data : GWindows.Types.Lparam);
   function Item_Data (List : List_Box_Type;
                       Item : Natural) return GWindows.Types.Lparam;
   --  To use user data with list box items

   function Count (List : in List_Box_Type) return Natural;
   --  Returns number of items in list box

   function Value_Length (List : in List_Box_Type;
                          Item : in Positive)
                         return Natural;
   --  Returns the length of string value at Item

   function Value (List : in List_Box_Type;
                   Item : in Positive)
                  return GString;
   --  Returns the string value at Item

   procedure Selected (List  : in out List_Box_Type;
                       Item  : in     Positive;
                       State : in     Boolean       := True);
   function Selected (List  : in List_Box_Type;
                      Item  : in Positive)
                     return Boolean;
   --  Item select state

   procedure Top_Item (List  : in out List_Box_Type;
                       Item  : in     Positive);
   function Top_Item (List  : in List_Box_Type) return Natural;
   --  Top item displayed in list box

   procedure Text (Window : in out List_Box_Type;
                   Text   : in     GString);

   function Text  (Window : in List_Box_Type)
                  return GString;
   --  Get or set current selection / focus item

   -------------------------------------------------------------------------
   --  List_Box_Type - Methods
   -------------------------------------------------------------------------

   procedure Add (List  : in out List_Box_Type;
                  Value : in     GString);
   function Add (List  : in List_Box_Type;
                 Value : in GString)
                return Natural;
   procedure Add (List  : in out List_Box_Type;
                  After : in     Positive;
                  Value : in     GString);
   --  Add a value to the list box

   procedure Delete (List : in out List_Box_Type;
                     Item : in     Positive);
   --  Delete value at item

   procedure Clear (List : in out List_Box_Type);
   --  Clear list box

   function Find (List             : in List_Box_Type;
                  Value            : in GString;
                  Start_After_Item : in Natural       := 0)
                 return Natural;
   --  Find a string in the list box, return its 1-based index.
   --  Start_After_Item is the one-based index of the item preceding the first
   --  item to be searched. When the search reaches the bottom of the list box,
   --  it continues from the top of the list box back to the item specified by
   --  the Start_After_Item parameter. If Start_After_Item is 0, the entire
   --  list box is searched from the beginning.
   --  Return 0 if not found.

   function Find_Exact (List             : in List_Box_Type;
                        Value            : in GString;
                        Start_After_Item : in Natural       := 0)
                       return Natural;
   --  Find a string in the list box, with an exact length match.
   --  Read above about Start_After_Item.
   --  Return 0 if not found.

   -------------------------------------------------------------------------
   --  List_Box_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Double_Click_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Double_Click (List : in out List_Box_Type);

   procedure On_Selection_Cancel_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Selection_Cancel (List : in out List_Box_Type);

   procedure On_Selection_Change_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Selection_Change (List : in out List_Box_Type);

   procedure On_Focus_Handler (List    : in out List_Box_Type;
                               Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Focus (List : in out List_Box_Type);

   procedure On_Lost_Focus_Handler
     (List    : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (List : in out List_Box_Type);

   procedure On_Out_Of_Memory_Handler
     (List : in out List_Box_Type;
      Handler : in     GWindows.Base.Action_Event);
   procedure Fire_On_Out_Of_Memory (List : in out List_Box_Type);

   -------------------------------------------------------------------------
   --  List_Box_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Double_Click (List : in out List_Box_Type);
   --  Double Clicked

   procedure On_Selection_Cancel (List : in out List_Box_Type);
   --  Selection Cancled

   procedure On_Selection_Change (List : in out List_Box_Type);
   --  Selection Changed

   procedure On_Focus (List : in out List_Box_Type);
   --  Received focus

   procedure On_Lost_Focus (List : in out List_Box_Type);
   --  Lost focus

   procedure On_Out_Of_Memory (List : in out List_Box_Type);
   --  Control can not allocate more memory

   -------------------------------------------------------------------------
   --  List_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Command (Window  : in out List_Box_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Receives command messags from parent window

   -------------------------------------------------------------------------
   --  Multiple_Selection_List_Box_Type
   -------------------------------------------------------------------------

   type Multiple_Selection_List_Box_Type is new List_Box_Type with private;
   type Multiple_Selection_List_Box_Access is
     access all Multiple_Selection_List_Box_Type;

   -------------------------------------------------------------------------
   --  Multiple_Selection_List_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List       : in out Multiple_Selection_List_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Sort       : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create List Box

   -------------------------------------------------------------------------
   --  Multiple_Selection_List_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Selected
     (List  : in out Multiple_Selection_List_Box_Type;
      Item  : in     Positive;
      State : in     Boolean       := True);

   procedure Select_Range
     (List       : in out Multiple_Selection_List_Box_Type;
      Start_Item : in     Positive;
      End_Item   : in     Positive;
      State      : in     Boolean                          := True);
   --  Select a range of items

   function Select_Count
     (List : in Multiple_Selection_List_Box_Type)
     return Natural;
   --  Number of selected items

   type Selection_Array_Type is array (Natural range <>) of aliased Integer;

   function Selected_Items
     (List : in Multiple_Selection_List_Box_Type)
     return Selection_Array_Type;

private
   type List_Box_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Double_Click_Event      : GWindows.Base.Action_Event := null;
         On_Selection_Cancel_Event  : GWindows.Base.Action_Event := null;
         On_Selection_Change_Event  : GWindows.Base.Action_Event := null;
         On_Focus_Event             : GWindows.Base.Action_Event := null;
         On_Lost_Focus_Event        : GWindows.Base.Action_Event := null;
         On_Out_Of_Memory_Event     : GWindows.Base.Action_Event := null;
      end record;

   type Multiple_Selection_List_Box_Type is new List_Box_Type with null record;

end GWindows.List_Boxes;
